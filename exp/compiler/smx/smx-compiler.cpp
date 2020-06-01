// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#include <algorithm>
#include <utility>

#include "compile-context.h"
#include "parser/ast.h"
#include "scopes.h"
#include "smx-compiler.h"
#include "array-helpers.h"
#include <amtl/am-maybe.h>
#include <amtl/am-string.h>
#include <smx/smx-v1.h>
#include <sp_vm_types.h>

#define __ masm_.

namespace sp {

static bool HasSimpleCellStorage(Type* type);
static cell_t GetCellFromBox(const BoxedValue& box);

typedef SmxListSection<sp_file_natives_t> SmxNativeSection;
typedef SmxListSection<sp_file_publics_t> SmxPublicSection;
typedef SmxListSection<sp_file_pubvars_t> SmxPubvarSection;
typedef SmxBlobSection<sp_file_data_t> SmxDataSection;
typedef SmxBlobSection<sp_file_code_t> SmxCodeSection;

uint64_t SValue::sSequenceNo = 0;

SmxCompiler::SmxCompiler(CompileContext& cc, sema::Program* program)
 : cc_(cc),
   program_(program),
   pri_value_(0),
   alt_value_(0),
   max_var_stk_(0),
   cur_var_stk_(0),
   heap_usage_(0),
   max_heap_usage_(0),
   loop_(nullptr),
   scope_info_(nullptr),
   last_stmt_pc_(0)
{
  names_ = new SmxNameTable(".names");
  builder_.add(names_);
}

bool
SmxCompiler::compile()
{
  // This is always the first opcode, so instruction 0 is invalid.
  __ opcode(OP_HALT);

  for (ast::VarDecl* decl : program_->globals) {
    if (!cc_.canContinueProcessing())
      return false;

    for (; decl; decl = decl->next())
      generateData(decl);
  }

  for (ast::FunctionStatement* fun : program_->functions) {
    if (!cc_.canContinueProcessing())
      return false;

    generate(fun);
  }

  if (masm_.outOfMemory()) {
    cc_.report(SourceLocation(), rmsg::outofmemory);
    return false;
  }

  assert(masm_.buffer_length() % sizeof(cell_t) == 0);

  add_code();
  add_data();
  add_natives();
  add_publics();
  add_pubvars();

  return cc_.phasePassed();
}

void
SmxCompiler::add_code()
{
  RefPtr<SmxCodeSection> code = new SmxCodeSection(".code");
  code->header().codesize = masm_.buffer_length();
  code->header().cellsize = sizeof(cell_t);
  code->header().codeversion = 13;
  code->header().flags = CODEFLAG_DEBUG;
  code->header().main = 0;
  code->header().code = sizeof(sp_file_code_t);
  code->header().features = SmxConsts::kCodeFeatureDirectArrays;
  code->setBlob(masm_.buffer(), masm_.buffer_length());
  builder_.add(code);
}

void
SmxCompiler::add_data()
{
  assert(data_.size() % sizeof(cell_t) == 0);

  // Ensure the data section has at least one value.
  // :TODO: clean up data handling, check <= INT_MAX, not overflowed
  if (!data_.size())
    data_.write(0);

  RefPtr<SmxDataSection> data = new SmxDataSection(".data");
  data->header().datasize = data_.size();
  data->header().memsize = data_.size() + 4096; // :TODO: real value
  data->header().data = sizeof(sp_file_data_t);
  data->setBlob(data_.bytes(), data_.size());
  builder_.add(data);
}

void
SmxCompiler::add_natives()
{
  if (natives_.size())
    qsort(natives_.data(), natives_.size(), sizeof(FunctionEntry), sort_functions);

  RefPtr<SmxNativeSection> natives =  new SmxNativeSection(".natives");
  for (size_t i = 0; i < natives_.size(); i++) {
    const FunctionEntry& entry = natives_[i];

    sp_file_natives_t& nf = natives->add();
    nf.name = names_->add(entry.name);

    __ bind_to(entry.fun->address(), (uint32_t)i);
  }
  builder_.add(natives);
}

void
SmxCompiler::add_publics()
{
  // :TODO: error
  assert(publics_.size());
  qsort(publics_.data(), publics_.size(), sizeof(FunctionEntry), sort_functions);

  RefPtr<SmxPublicSection> publics = new SmxPublicSection(".publics");
  for (size_t i = 0; i < publics_.size(); i++) {
    const FunctionEntry& entry = publics_[i];

    sp_file_publics_t& pf = publics->add();
    pf.address = entry.fun->address()->offset();
    pf.name = names_->add(entry.name);
  }
  builder_.add(publics);
}

static int
sort_vars(const void* a1, const void* a2)
{
  ast::VarDecl* var1 = *(ast::VarDecl**)a1;
  ast::VarDecl* var2 = *(ast::VarDecl**)a2;
  return strcmp(var1->name()->chars(), var2->name()->chars());
}

void
SmxCompiler::add_pubvars()
{
  qsort(program_->globals.data(), program_->globals.size(), sizeof(ast::VarDecl*), sort_vars);

  RefPtr<SmxPubvarSection> pubvars = new SmxPubvarSection(".pubvars");
  for (ast::VarDecl* decl : program_->globals) {
    if (decl->classifier() != TOK_PUBLIC)
      continue;

    sp_file_pubvars_t& pv = pubvars->add();
    pv.address = decl->sym()->address();
    pv.name = names_->add(decl->name());
  }
  if (!pubvars->length())
    return;
  builder_.add(pubvars);
}

bool
SmxCompiler::emit(ISmxBuffer* buffer)
{
  return builder_.write(buffer);
}

void
SmxCompiler::generate(ast::FunctionStatement* fun)
{
  // :TODO: move this into local state, otherwise nested functions will be hard.
  max_var_stk_ = 0;
  cur_var_stk_ = 0;
  entry_stack_op_.reset();

  __ bind(fun->address());
  __ opcode(OP_PROC);

    // :TODO:  cap input at INT_MAX bytes to implicitly limit switch cases/args
    // then remove too_many_cases

  // Allocate arguments.
  static const size_t kFirstArgSlot = 3;
  static const size_t kMaxArgSlots = (INT_MAX / 4) - kFirstArgSlot;

  ast::FunctionSignature* sig = fun->signature();
  ast::ParameterList* params = sig->parameters();
  if (params->size() >= kMaxArgSlots)
    cc_.report(fun->loc(), rmsg::too_many_arguments);

  for (size_t i = 0; i < params->size(); i++) {
    VariableSymbol* param = params->at(i)->sym();
    param->allocate(StorageClass::Argument, (i + kFirstArgSlot) * sizeof(cell_t));
  }

  // :TODO: don't generate if no local vars
  __ opcode(OP_STACK, &entry_stack_op_);

  generateBlock(fun->body());

  __ bind_to(&entry_stack_op_, -max_var_stk_);

  if (!fun->guaranteed_return()) {
    __ opcode(OP_ZERO_PRI);
    __ opcode(OP_RETN);
  }
  __ opcode(OP_ENDPROC);

  Atom* name = fun->name();
  if (fun->token() != TOK_PUBLIC) {
    // We add all functions to the publics table as a great compatibility hack:
    // it makes the functions invisible (and hard to reliably predict) to other
    // plugins, as well as the VM, but lets us use the ancient FunctionId scheme
    // for passing functions-as-values.
    //
    // This decoration is totally random, it could be anything, as long as it's
    // likely to change across compilations.
    //
    // Note: We really need a better string library.
    char decorated_prefix[24];
    ke::SafeSprintf(decorated_prefix, sizeof(decorated_prefix), ".%d.", fun->address()->offset());

    size_t length = strlen(decorated_prefix) + name->length() + 1;
    std::unique_ptr<char[]> decorated(new char[length]);
    ke::SafeSprintf(decorated.get(), length, "%s%s", decorated_prefix, name->chars());
    name = cc_.add(decorated.get(), length);
  }

  publics_.push_back(FunctionEntry(name, fun));
}

void
SmxCompiler::generateStatement(ast::Statement* stmt)
{
  // We don't really have line information here, so we emit BREAKs at
  // statements instead.
  if (last_stmt_pc_ != masm_.pc()) {
    __ opcode(OP_BREAK);
    last_stmt_pc_ = masm_.pc();
  }

  assert(heap_usage_ == 0);

  // :TODO: track stack depth
  switch (stmt->kind()) {
    case ast::AstKind::kReturnStatement:
      generateReturn(stmt->toReturnStatement());
      break;
    case ast::AstKind::kExpressionStatement:
      generateExprStatement(stmt->toExpressionStatement());
      break;
    case ast::AstKind::kVarDecl:
    {
      for (ast::VarDecl* decl = stmt->toVarDecl(); decl; decl = decl->next())
        generateVarDecl(decl);
      break;
    }
    case ast::AstKind::kWhileStatement:
      generateWhile(stmt->toWhileStatement());
      break;
    case ast::AstKind::kForStatement:
      generateFor(stmt->toForStatement());
      break;
    case ast::AstKind::kIfStatement:
      generateIf(stmt->toIfStatement());
      break;
    case ast::AstKind::kBlockStatement:
      generateBlock(stmt->toBlockStatement());
      break;
    case ast::AstKind::kBreakStatement:
      generateBreak(stmt->toBreakStatement());
      break;
    case ast::AstKind::kContinueStatement:
      generateContinue(stmt->toContinueStatement());
      break;
    case ast::AstKind::kSwitchStatement:
      generateSwitch(stmt->toSwitchStatement());
      break;
    default:
      cc_.report(stmt->loc(), rmsg::unimpl_kind) <<
        "smx-gen-stmt" << stmt->kindName();
      return;
  }

  // If compilation is going okay, the operand stack should be empty.
  if (cc_.phasePassed() &&
      (!operand_stack_.empty() || pri_value_ || alt_value_))
  {
    cc_.report(SourceLocation(), rmsg::regalloc_error) <<
      "operand stack is not empty at end of statement";
  }

  if (heap_usage_) {
    __ opcode(OP_HEAP, -heap_usage_);
    max_heap_usage_ = std::max(heap_usage_, max_heap_usage_);
    heap_usage_ = 0;
  }
}

void
SmxCompiler::generateBlock(ast::BlockStatement* block)
{
  ScopeInfo scope_info(this, &scope_info_);

  // :TODO: raii this, so for loops can use it for the init scope
  int32_t stk_usage = cur_var_stk_;

  ast::StatementList* statements = block->statements();
  for (size_t i = 0; i < statements->size(); i++) {
    ast::Statement* stmt = statements->at(i);
    generateStatement(stmt);
  }

  // Commit this scope's stack usage to the frame, then drop our local usage.
  max_var_stk_ = std::max(max_var_stk_, cur_var_stk_);
  cur_var_stk_ = stk_usage;
}

void
SmxCompiler::generateVarDecl(ast::VarDecl* stmt)
{
  // :TODO: assert not const, figure out what to do
  VariableSymbol* sym = stmt->sym();

  // :TODO: obey decl if we can

  int32_t size;

  ArrayInfo array_info;
  ArrayType* array_type = sym->type()->isArray()
                          ? sym->type()->toArray()
                          : nullptr;
  if (array_type && array_type->hasFixedLength()) {
    if (!ComputeArrayInfo(array_type, &array_info)) {
      cc_.report(stmt->loc(), rmsg::array_too_big);
      return;
    }
    size = array_info.bytes;
  } else {
    size = compute_storage_size(sym->type());
  }

  assert(ke::IsAligned(size_t(size), sizeof(cell_t)));

  switch (sym->scope()->kind()) {
    case Scope::Block:
      if (!ke::IsUint32AddSafe(uint32_t(size), uint32_t(cur_var_stk_))) {
        cc_.report(stmt->loc(), rmsg::too_much_stack);
        return;
      }

      cur_var_stk_ += size;
      sym->allocate(StorageClass::Local, -cur_var_stk_);
      break;

    default:
      cc_.report(stmt->loc(), rmsg::unimpl_kind) <<
        "smx-var-scope" << int32_t(sym->scope()->kind());
      sym->allocate(StorageClass::Local, 0);
      break;
  }

  sema::Expr* init = stmt->sema_init();

  if (sym->type()->isArray()) {
    ArrayType* at = sym->type()->toArray();
    if (at->hasFixedLength())
      initialize_array(sym, init, array_info);
    else
      initialize_dynamic_array(stmt, init);
    return;
  }

  // If we don't have an array, we should be assigning a simple type.
  assert(HasSimpleCellStorage(sym->type()));

  // Small optimization - treat constant zero as no initializer. We'll
  // fallthrough below. :TODO: make sure this works for floats.
  int32_t value;
  if (init && init->getConstantInt32(&value) && !value)
    init = nullptr;

  if (init) {
    if (!emit_into(init, ValueDest::Pri))
      return;

    emit_var_store(sym, ValueDest::Pri);
    return;
  }

  // No initializer, do it manually.
  switch (sym->storage()) {
    case StorageClass::Argument:
    case StorageClass::Local:
      __ opcode(OP_ZERO_S, sym->address());
      break;

    default:
      cc_.report(stmt->loc(), rmsg::unimpl_kind) <<
        "unexpected storage class" << int32_t(sym->storage());
  }
}

void
SmxCompiler::generateExprStatement(ast::ExpressionStatement* stmt)
{
  sema::Expr* expr = stmt->sema_expr();
  emit_into(expr, ValueDest::Pri);
}

void
SmxCompiler::generateReturn(ast::ReturnStatement* stmt)
{
  // Note: functions with void return return 0 for compatibility reasons. We
  // could probably do away with this for non-publics with a void return.
  if (sema::Expr* expr = stmt->sema_expr())
    emit_into(expr, ValueDest::Pri);
  else
    __ opcode(OP_ZERO_PRI);
  __ opcode(OP_RETN);
}

void
SmxCompiler::generateWhile(ast::WhileStatement* stmt)
{
  sema::Expr* cond = stmt->sema_cond();

  LoopScope loop(&loop_, scope_info_);

  Label taken, fallthrough;
  if (stmt->token() == TOK_WHILE) {
    loop.continue_to = &fallthrough;
    loop.break_to = &taken;

    // if !<cond> goto done
    // repeat:
    //   <body>
    //   jump repeat
    // done:
    test(cond, false, &taken, &fallthrough);
    __ bind(&fallthrough);
    generateStatement(stmt->body());
    __ opcode(OP_JUMP, &fallthrough);
    __ bind(&taken);
  } else {
    loop.continue_to = &taken;
    loop.break_to = &fallthrough;

    // repeat:
    //   <body>
    //   if <cond> goto repeat
    // done:
    assert(stmt->token() == TOK_DO);
    __ bind(&taken);
    generateStatement(stmt->body());
    test(cond, true, &taken, &fallthrough);
    __ bind(&fallthrough);
  }
}

void
SmxCompiler::generateFor(ast::ForStatement* stmt)
{
  ScopeInfo scope_info(this, &scope_info_);
  if (ast::Statement* init = stmt->initialization())
    generateStatement(init);

  Label cond, body, done, update;

  // cond:
  //  test expr
  //  jfalse done
  // body:
  //  <body>
  // update:
  //  <update>
  //  jmp <cond>
  // done:

  __ bind(&cond);
  if (sema::Expr* cond_expr = stmt->sema_cond())
    test(cond_expr, false, &done, &body);
  __ bind(&body);
  {
    LoopScope loop(&loop_, scope_info_);
    loop.continue_to = &update;
    loop.break_to = &done;

    ast::Statement* body = stmt->body();
    generateStatement(body);
  }
  __ bind(&update);
  if (ast::Statement* update_stmt = stmt->update())
    generateStatement(update_stmt);
  __ opcode(OP_JUMP, &cond);
  __ bind(&done);
}

void
SmxCompiler::generateIf(ast::IfStatement* stmt)
{
  Label taken, fallthrough;

  for (size_t i = 0; i < stmt->clauses()->size(); i++) {
    const ast::IfClause& clause = stmt->clauses()->at(i);

    // The previous false jump goes to the next condition.
    __ bind(&taken);
    taken.reset();

    test(clause.sema_cond, false, &taken, &fallthrough);
    __ bind(&fallthrough);
    generateStatement(clause.body);
    fallthrough.reset();
  }

  __ bind(&taken);
  if (ast::Statement* final_else = stmt->fallthrough())
    generateStatement(final_else);
}

void
SmxCompiler::generateBreak(ast::BreakStatement* stmt)
{
  jump_to_scope(loop_->scope_info);
  __ opcode(OP_JUMP, loop_->break_to);
}

void
SmxCompiler::generateContinue(ast::ContinueStatement* stmt)
{
  jump_to_scope(loop_->scope_info);
  __ opcode(OP_JUMP, loop_->continue_to);
}

void
SmxCompiler::generateSwitch(ast::SwitchStatement* stmt)
{
  PoolList<ast::Case*>* cases = stmt->cases();

  size_t total_cases = 0;
  for (size_t i = 0; i < cases->size(); i++) {
    ast::Case* entry = cases->at(i);
    total_cases += entry->values()->size();
  }

  if (total_cases >= INT_MAX) {
    cc_.report(stmt->loc(), rmsg::too_many_cases);
    return;
  }

  if (!emit_into(stmt->sema_expr(), ValueDest::Pri))
    return;

  Label casetbl, defcase;
  __ opcode(OP_SWITCH, &casetbl);
  __ bind(&casetbl);
  __ casetbl(total_cases, &defcase);

  std::unique_ptr<Label[]> labels(new Label[cases->size()]);

  // Generate entries for the CASETBL opcode above.
  for (size_t i = 0; i < cases->size(); i++) {
    ast::Case* entry = cases->at(i);

    for (size_t j = 0; j < entry->values()->size(); j++) {
      cell_t value = entry->values()->at(j);
      __ casetbl_entry(value, &labels[i]);
    }
  }

  Label done;

  // Now generate the actual switch cases.
  for (size_t i = 0; i < cases->size(); i++) {
    ast::Case* entry = cases->at(i);

    __ bind(&labels[i]);
    generateStatement(entry->statement());

    // All but the very last case should jump past the other cases.
    if (i != cases->size() - 1 || stmt->defaultCase())
      __ opcode(OP_JUMP, &done);
  }

  __ bind(&defcase);
  if (ast::Statement* defaultCase = stmt->defaultCase())
    generateStatement(defaultCase);
  __ bind(&done);
}

ValueDest
SmxCompiler::emit(sema::Expr* expr, ValueDest dest)
{
  switch (expr->kind()) {
  case sema::ExprKind::ConstValue:
    return emitConstValue(expr->toConstValueExpr(), dest);
  case sema::ExprKind::Ternary:
    return emitTernary(expr->toTernaryExpr(), dest);
  case sema::ExprKind::Binary:
    return emitBinary(expr->toBinaryExpr(), dest);
  case sema::ExprKind::Unary:
    return emitUnary(expr->toUnaryExpr(), dest);
  case sema::ExprKind::Call:
    return emitCall(expr->toCallExpr(), dest);
  case sema::ExprKind::Var:
    return emitVar(expr->toVarExpr(), dest);
  case sema::ExprKind::ImplicitCast:
    return emitImplicitCast(expr->toImplicitCastExpr(), dest);
  case sema::ExprKind::String:
    return emitString(expr->toStringExpr(), dest);
  case sema::ExprKind::IncDec:
    return emitIncDec(expr->toIncDecExpr(), dest);
  case sema::ExprKind::Index:
    return emitIndex(expr->toIndexExpr(), dest);
  case sema::ExprKind::Load:
    return emitLoad(expr->toLoadExpr(), dest);
  case sema::ExprKind::Store:
    return emitStore(expr->toStoreExpr(), dest);
  default:
    cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
      "smx-emit-expr" << expr->prettyName();
    return ValueDest::Error;
  }
}

// Emit an expression, ensuring it's in the desired location.
bool
SmxCompiler::emit_into(sema::Expr* expr, ValueDest dest)
{
  ValueDest actual = emit(expr, dest);
  if (actual == ValueDest::Error)
    return false;
  if (actual == dest)
    return true;

  will_kill(dest);
  if (dest == ValueDest::Pri || dest == ValueDest::Alt) {
    // move.pri = pri <- alt
    // move.alt = alt <- pri
    //
    // Since actual != dest, we just need to move into the opposite register.
    if (actual == ValueDest::Alt)
      __ opcode(OP_MOVE_PRI);
    else
      __ opcode(OP_MOVE_ALT);
    return true;
  }

  assert(dest == ValueDest::Stack);
  if (actual == ValueDest::Pri)
    __ opcode(OP_PUSH_PRI);
  else if (actual == ValueDest::Alt)
    __ opcode(OP_PUSH_ALT);
  else
    assert(false);

  return true;
}

ValueDest
SmxCompiler::emitConstValue(sema::ConstValueExpr* expr, ValueDest dest)
{
  cell_t value = 0;
  const BoxedValue& box = expr->value();

  switch (box.kind()) {
    case BoxedValue::Kind::Integer:
    {
      const IntValue& iv = box.toInteger();
      assert(iv.valueFitsInInt32());
      value = (int32_t)iv.asSigned();
      break;
    }
    default:
      assert(false);
  }

  emit_const(dest, value);
  return dest;
}

ValueDest
SmxCompiler::emitLoad(sema::LoadExpr* expr, ValueDest dest)
{
  sema::LValueExpr* lvalue = expr->lvalue()->asLValueExpr();
  switch (lvalue->kind()) {
    case sema::ExprKind::Var:
    {
      // :TODO: rm storage flags
      sema::VarExpr* var_expr = lvalue->toVarExpr();
      return emit_var_load(var_expr, dest);
    }

    case sema::ExprKind::Index:
    {
      if (!emit_into(lvalue, ValueDest::Pri))
        return ValueDest::Error;
      if (lvalue->type()->isPrimitive(PrimitiveType::Char))
        __ opcode(OP_LODB_I, (cell_t)1);
      else
        __ opcode(OP_LOAD_I);
      return ValueDest::Pri;
    }

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "emit-load-kind" << lvalue->prettyName();
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emitStore(sema::StoreExpr* expr, ValueDest requested)
{
  sema::LValueExpr* lvalue = expr->left()->asLValueExpr();
  sema::Expr* right = expr->right();

  // Only non-composite types can occur here.
  assert(HasSimpleCellStorage(UnwrapReference(lvalue->type())));
  assert((size_t)compute_storage_size(UnwrapReference(lvalue->type())) <= sizeof(cell_t));

  // There are many special case opcodes for assigning to variables.
  if (sema::VarExpr* var = lvalue->asVarExpr()) {
    // We need a temporary register, so we have to ignore stack requests.
    ValueDest dest = requested;
    if (dest == ValueDest::Stack)
      dest = ValueDest::Pri;

    if (!emit_into(right, dest))
      return ValueDest::Error;

    emit_var_store(var->sym(), dest);
    return dest;
  }

  // No special cases are available for other l-value types. Use addresses.
  if (!emit_into(lvalue, ValueDest::Alt))
    return ValueDest::Error;

  uint64_t saved_alt = preserve(ValueDest::Alt);
  if (!emit_into(right, ValueDest::Pri))
    return ValueDest::Error;
  restore(saved_alt);

  // Ideally, the VM would have enough type information to perform the correct
  // kind of store. But it doesn't, so we need to detect the "magic string" hack
  // the old compiler used. It's not really a hack here, except that IndexExpr
  // is the only pointer-type for which this storage rule exists (rather than
  // all addresses having a consistent storage type).
  Type* type = lvalue->type();
  if (type->isPrimitive(PrimitiveType::Char) && lvalue->asIndexExpr())
    __ opcode(OP_STRB_I, (cell_t)1);
  else
    __ opcode(OP_STOR_I);
  return ValueDest::Pri;
}

static inline ke::Maybe<int32_t>
MaybeConstInt32(sema::Expr* expr)
{
  sema::ConstValueExpr* cv = expr->asConstValueExpr();
  if (!cv)
    return Nothing();

  const BoxedValue& box = cv->value();
  if (box.kind() != BoxedValue::Kind::Integer)
    return Nothing();

  const IntValue& iv = box.toInteger();
  if (!iv.valueFitsInInt32())
    return Nothing();

  return Some((int32_t)iv.asSigned());
}

ValueDest
SmxCompiler::emitBinary(sema::BinaryExpr* expr, ValueDest dest)
{
  sema::Expr* left = expr->left();
  sema::Expr* right = expr->right();

  Maybe<int32_t> left_i32 = MaybeConstInt32(left);
  Maybe<int32_t> right_i32 = MaybeConstInt32(right);

  // Special case SHL since it has a specific optimization and it's gross to
  // fall-through case statements in a complex switch.
  if (expr->token() == TOK_SHL && right_i32) {
    // If we don't have to push to the stack, we can pick exactly which
    // register was requested.
    ValueDest actual = (dest == ValueDest::Stack)
                       ? ValueDest::Pri
                       : dest;
    if (!emit_into(left, actual))
      return ValueDest::Error;

    if (actual == ValueDest::Pri)
      __ opcode(OP_SHL_C_PRI, *right_i32);
    else
      __ opcode(OP_SHL_C_ALT, *right_i32);
    return actual;
  }

  switch (expr->token()) {
    case TOK_PLUS:
    case TOK_STAR:
    case TOK_EQUALS:
    {
      // Try to get left=expr and right=const for commutative operations.
      if (left_i32 && !right_i32 &&
          (expr->token() == TOK_PLUS || expr->token() == TOK_STAR))
      {
        std::swap(left, right);
        std::swap(left_i32, right_i32);
      }

      if (!emit_into(left, ValueDest::Pri))
        return ValueDest::Error;

      // Use .C variants.
      if (right_i32) {
        if (expr->token() == TOK_PLUS)
          __ opcode(OP_ADD_C, *right_i32);
        else if (expr->token() == TOK_STAR)
          __ opcode(OP_SMUL_C, *right_i32);
        else if (expr->token() == TOK_EQUALS)
          __ opcode(OP_EQ_C_PRI, *right_i32);
        return ValueDest::Pri;
      }

      uint64_t saved_pri = preserve(ValueDest::Pri);
      if (!emit_into(right, ValueDest::Alt))
        return ValueDest::Error;
      restore(saved_pri);

      if (expr->token() == TOK_PLUS)
        __ opcode(OP_ADD);
      else if (expr->token() == TOK_STAR)
        __ opcode(OP_SMUL);
      else if (expr->token() == TOK_EQUALS)
        __ opcode(OP_EQ);
      return ValueDest::Pri;
    }

    case TOK_MINUS:
    case TOK_BITOR:
    case TOK_BITXOR:
    case TOK_BITAND:
    case TOK_SHL:
    case TOK_SHR:
    case TOK_USHR:
    case TOK_NOTEQUALS:
    case TOK_GT:
    case TOK_GE:
    case TOK_LT:
    case TOK_LE:
    case TOK_SLASH:
    case TOK_PERCENT:
    {
      if (!load_both(left, right))
        return ValueDest::Error;

      switch (expr->token()) {
        case TOK_MINUS:
          __ opcode(OP_SUB);
          break;
        case TOK_BITOR:
          __ opcode(OP_OR);
          break;
        case TOK_BITXOR:
          __ opcode(OP_XOR);
          break;
        case TOK_BITAND:
          __ opcode(OP_AND);
          break;
        case TOK_SHL:
          __ opcode(OP_SHL);
          break;
        case TOK_SHR:
          __ opcode(OP_SSHR);
          break;
        case TOK_USHR:
          __ opcode(OP_SHR);
          break;
        case TOK_NOTEQUALS:
          __ opcode(OP_NEQ);
          break;
        case TOK_GT:
          __ opcode(OP_SGRTR);
          break;
        case TOK_GE:
          __ opcode(OP_SGEQ);
          break;
        case TOK_LT:
          __ opcode(OP_SLESS);
          break;
        case TOK_LE:
          __ opcode(OP_SLEQ);
          break;
        case TOK_SLASH:
        case TOK_PERCENT:
          __ opcode(OP_SDIV);
          break;
        default:
          assert(false);
          break;
      }
      if (expr->token() == TOK_PERCENT)
        return ValueDest::Alt;
      return ValueDest::Pri;
    }

    case TOK_AND:
    case TOK_OR:
    {
      Label done, taken, fallthrough;

      // Make sure we've killed pri/alt on all branches. This may result in
      // unnecessary spills! We don't really have any way to avoid that
      // without pre-analyzing |expr|.
      will_kill(ValueDest::Pri);
      will_kill(ValueDest::Alt);

      // Note: test implements special optimizations for || and && and we
      // re-use them here. If test did not do this, it would infinitely
      // recurse below. If those optimizations go away, we would need to
      // test left/right independently here.
      test(expr, true, &taken, &fallthrough);
      __ bind(&fallthrough);
      emit_const(dest, 0);
      __ opcode(OP_JUMP, &done);
      emit_const(dest, 1);
      __ bind(&done);
      return dest;
    }

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "smx-binexpr-tok" << TokenNames[expr->token()];
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emitUnary(sema::UnaryExpr* expr, ValueDest dest)
{
  sema::Expr* inner = expr->expr();

  switch (expr->token()) {
    case TOK_NEGATE:
    case TOK_NOT:
    case TOK_TILDE:
      if (!emit_into(inner, ValueDest::Pri))
        return ValueDest::Error;

      switch (expr->token()) {
        case TOK_NEGATE:
          __ opcode(OP_NEG);
          break;
        case TOK_NOT:
          __ opcode(OP_NOT);
          break;
        case TOK_TILDE:
          __ opcode(OP_INVERT);
          break;
        default:
          assert(false);
      }
      return ValueDest::Pri;

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "smx-unary-tok" << TokenNames[expr->token()];
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emitTernary(sema::TernaryExpr* expr, ValueDest dest)
{
  // If the final destination is to the stack, we assert that the following
  // kills will not cause any spills. Otherwise, the ordering of the stack
  // would become corrupt. We can assert this is true since only function
  // calls will use ValueDest::Stack, and they should be guaranteeing
  // Pri and Alt are unused between arguments.
  if (dest == ValueDest::Stack)
    assert(pri_value_ == 0 && alt_value_ == 0);

  // We kill pri and alt ahead of time, since it would be illegal to kill it on
  // one branch and not the other. We need some more advanced tech to do better.
  will_kill(ValueDest::Pri);
  will_kill(ValueDest::Alt);

  Label false_case, fallthrough, done;

  test(expr->choose(), false, &false_case, &fallthrough);
  __ bind(&fallthrough);
  if (!emit_into(expr->left(), dest))
    return ValueDest::Error;
  __ opcode(OP_JUMP, &done);

  __ bind(&false_case);
  if (!emit_into(expr->right(), dest))
    return ValueDest::Error;

  __ bind(&done);
  return dest;
}

ValueDest
SmxCompiler::emitImplicitCast(sema::ImplicitCastExpr* expr, ValueDest requested)
{
  switch (expr->op()) {
    case sema::CastOp::None:
      return emit(expr->expr(), requested);

    case sema::CastOp::TruncateInt:
    {
      if (!emit_into(expr->expr(), ValueDest::Pri))
        return ValueDest::Error;

      switch (expr->type()->primitive()) {
        case PrimitiveType::Char:
          // Right now, we do not implement this, because the storage type of
          // a char is int32 when on the stack. We need to change this to be
          // consistent with semantic analysis, but today's not the day.
          // By leaving this unimplemented, we're preserving old behavior.
          return ValueDest::Pri;

        default:
          cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
            "smx-cast-trunc" << expr->type();
          return ValueDest::Error;
      }
    }

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "smx-cast-op" << unsigned(expr->op());
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emitCall(sema::CallExpr* expr, ValueDest dest)
{
  // We only support named callees right now (that is, the function cannot be
  // stored in a variable or as the result of an expression).
  sema::NamedFunctionExpr* callee = expr->callee()->asNamedFunctionExpr();
  assert(callee);

  FunctionSymbol* fun = callee->sym();
  ast::FunctionSignature* sig = fun->impl()->signature();

  // SMX v1 requires that variadic arguments be passed by-reference, for no
  // discernable reason.
  size_t formal_argc = sig->parameters()->size();
  if (!sig->parameters()->empty() &&
      sig->parameters()->back()->sym()->type()->isVariadic())
  {
    formal_argc = sig->parameters()->size() - 1;
  }

  // We have to kill pri/alt before entering the argument push sequence, since
  // otherwise we may misalign the arguments.
  will_kill(ValueDest::Pri);
  will_kill(ValueDest::Alt);

  // SourcePawn evaluates arguments right-to-left, probably not for any
  // semantic reason, but because of how the original compiler worked.
  // The authors wanted arguments to be laid out on the stack such that
  // argument 0 would be at address +0, argument 1 at address +4, etc,
  // probably to make handling variadic arguments slightly easier. Instead
  // of generating a series of moves, it would instead put markers around
  // the instruction stream where each individual argument was generated.
  // Then, it would reorder everything in between these markers, so that
  // everything pushed to the stack in the right order.
  //
  // It's not clear why this was chosen over moves - possibly it made
  // the code generator simpler, or possibly the compiler was initially
  // one-pass (and since it never had an AST, it wouldn't have known the
  // argument count).

  // TODO make sure we verify that call labels are bound
  sema::ExprList* args = expr->args();

  // This would overflow the PUSH_C opcode below.
  static const size_t kMaxArgs = (INT_MAX / sizeof(cell_t)) - 1;
  if (args->size() > kMaxArgs)
    cc_.report(expr->src()->loc(), rmsg::too_many_arguments);

  for (size_t i = args->size() - 1; i < args->size(); i--) {
    sema::Expr* expr = args->at(i);

    size_t opstack_size = operand_stack_.size();

    if (i >= formal_argc &&
        !expr->isAddressable() &&
        !expr->type()->isAddressable())
    {
      // :TODO: write peephole optimization for f(n++) to not lose the lvalue.
      assert(HasSimpleCellStorage(expr->type()));
      if (!emit_into(expr, ValueDest::Pri))
        return ValueDest::Error;

      __ opcode(OP_HEAP, sizeof(cell_t));
      __ opcode(OP_STOR_I);
      __ opcode(OP_PUSH_ALT);
      heap_usage_ += sizeof(cell_t);
    } else {
      if (!emit_into(expr, ValueDest::Stack))
        return ValueDest::Error;
    }

    // Make sure emit_into does not cause any spills (or, if it did, that the
    // spills were cleaned up internally). Otherwise the stack will be
    // misaligned.
    if (opstack_size != operand_stack_.size()) {
      cc_.report(SourceLocation(), rmsg::regalloc_error) <<
        "argument pushed too many values onto the stack";
    }
  }

  if (sig->native()) {
    // Mark the native as used.
    if (!fun->impl()->address()->used())
      natives_.push_back(FunctionEntry(fun->name(), fun->impl()));

    __ sysreq_n(fun->impl()->address(), (uint32_t)args->size());
    assert(fun->impl()->address()->used());
  } else {
    __ opcode(OP_PUSH_C, cell_t(args->size()));
    __ opcode(OP_CALL, fun->impl()->address());
  }

  return ValueDest::Pri;
}

void
SmxCompiler::generateData(ast::VarDecl* decl)
{
  VariableSymbol* sym = decl->sym();
  if (sym->type()->isStruct()) {
    generateLegacyStructData(decl);
    return;
  }

  int32_t address = int32_t(data_.position());
  sym->allocate(StorageClass::Global, address);

  if (sym->type()->isArray()) {
    ArrayType* array = sym->type()->toArray();
    if (!array->hasFixedLength()) {
      // This should never happen.
      assert(false);
      return;
    }

    ArrayInfo info;
    if (!ComputeArrayInfo(array, &info)) {
      cc_.report(sym->node()->loc(), rmsg::array_too_big);
      return;
    }

    initialize_array(sym, decl->sema_init(), info);
    return;
  }

  if (!data_.write<cell_t>(0))
    return;

  sema::Expr* init = decl->sema_init();
  if (!init)
    return;

  uint8_t* bytes = data_.bytes() + sym->address();
  switch (init->kind()) {
    case sema::ExprKind::ConstValue:
    {
      // :TODO: use getBoxedValue and getCellBox
      sema::ConstValueExpr* expr = init->toConstValueExpr();
      const BoxedValue& value = expr->value();
      if (value.isInteger()) {
        *reinterpret_cast<int32_t*>(bytes) = (int32_t)value.toInteger().asSigned();
      } else {
        cc_.report(decl->loc(), rmsg::unimpl_kind) <<
          "smx-gen-data" << "value type";
      }
      break;
    }
  }
}

void
SmxCompiler::generateLegacyStructData(ast::VarDecl* stmt)
{
  VariableSymbol* sym = stmt->sym();
  StructType* st = sym->type()->asStruct();
  ast::RecordDecl* decl = st->decl();
  ast::LayoutDecls* body = decl->body();

  sema::StructInitExpr* init = stmt->sema_init()
                               ? stmt->sema_init()->toStructInitExpr()
                               : nullptr;

  std::vector<int32_t> values;

  size_t field_index = 0;
  for (ast::LayoutDecl* decl : *body) {
    ast::FieldDecl* field = decl->asFieldDecl();
    if (!field)
      continue;

    sema::Expr* expr = nullptr;
    if (init)
      expr = init->exprs()->at(field_index);

    FieldSymbol* sym = field->sym();
    int32_t value;
    if (sym->type()->isCharArray()) {
      if (expr) {
        Atom* atom = expr->toStringExpr()->literal();
        value = generateDataString(atom->chars(), atom->length());
       } else {
        value = generateDataString("", 0);
       }
    } else {
      if (expr)
        value = expr->toConstValueExpr()->value().toInteger().asSigned();
      else
        value = 0;
    }

    values.push_back(value);
    field_index++;
  }

  int32_t address = int32_t(data_.position());
  sym->allocate(StorageClass::Global, address);

  for (int32_t value : values)
    data_.write<cell_t>(value);
}

ValueDest
SmxCompiler::emitString(sema::StringExpr* expr, ValueDest dest)
{
  Atom* literal = expr->literal();

  // The array size should be bytes + 1, for the null terminator.
  assert(size_t(expr->type()->toArray()->fixedLength()) == literal->length() + 1);

  int32_t address = generateDataString(literal->chars(), literal->length());
  emit_const(dest, address);
  return dest;
}

int32_t
SmxCompiler::generateDataString(const char* str, size_t length)
{
  int32_t address = int32_t(data_.position());

  // Allocations must be in increments of cells.
  size_t leftover = Align(length + 1, sizeof(cell_t)) - (length + 1);
  assert(leftover < sizeof(cell_t));

  // Write the string, make sure we're still aligned after. Note we don't do
  // string coalescing (yet), it would work for general cases but not this:
  //
  //    void blah(char[] a="")
  //
  // In this case we need to guarantee a new literal copy each time the
  // default argument is used. We should be able to mark the StringExpr
  // as coalescable and fix this in the future.
  data_.writeBytes(str, length);
  data_.write<uint8_t>(0);
  for (size_t i = 0; i < leftover; i++)
    data_.write<uint8_t>(0);
  assert(ke::IsAligned(data_.size(), sizeof(cell_t)));

  return address;
}

#if 0
// Return alt if reg is pri, otherwise return pri. This works even for Stack,
// since usually we just want a register to spill.
static inline ValueDest
pick_other(ValueDest reg)
{
  if (reg == ValueDest::Pri)
    return ValueDest::Alt;
  return ValueDest::Pri;
}
#endif

ValueDest
SmxCompiler::emitIncDec(sema::IncDecExpr* expr, ValueDest dest)
{
  sema::LValueExpr* lvalue = expr->expr();

  // We have all sorts of special cases for global variables.
  sema::VarExpr* var = lvalue->asVarExpr();
  if (var && !var->sym()->type()->isReference()) {
    VariableSymbol* sym = var->sym();

    if (expr->postfix())
      emit_var_load(var, dest);

    switch (sym->storage()) {
      case StorageClass::Argument:
      case StorageClass::Local:
        if (expr->token() == TOK_INCREMENT)
          __ opcode(OP_INC_S, sym->address());
        else
          __ opcode(OP_DEC_S, sym->address());
        break;
      case StorageClass::Global:
        if (expr->token() == TOK_INCREMENT)
          __ opcode(OP_INC, sym->address());
        else
          __ opcode(OP_DEC, sym->address());
        break;
      default:
        assert(false);
    }

    if (expr->prefix())
      emit_var_load(var, dest);
    return dest;
  }

  will_kill(ValueDest::Pri);
  will_kill(ValueDest::Alt);

  if (lvalue->type()->isPrimitive(PrimitiveType::Char)) {
    if (!emit_into(lvalue, ValueDest::Pri))
      return ValueDest::Error;

    OPCODE incdec_op = (expr->token() == TOK_INCREMENT)
                       ? OP_INC_PRI
                       : OP_DEC_PRI;
    __ opcode(OP_MOVE_ALT);
    __ opcode(OP_LODB_I, (cell_t)1);
    if (expr->postfix())
      __ opcode(OP_PUSH_PRI);
    __ opcode(incdec_op);
    __ opcode(OP_STRB_I, (cell_t)1);
    if (expr->postfix()) {
      // Note: ValueDest::Stack is implicitly handled by the push above.
      if (dest == ValueDest::Pri)
        __ opcode(OP_POP_PRI);
      else if (dest == ValueDest::Alt)
        __ opcode(OP_POP_ALT);
      return dest;
    }
    return ValueDest::Pri;
  }

  // Otherwise, emit an address and use INC/DEC_I.
  if (!emit_into(lvalue, ValueDest::Pri))
    return ValueDest::Error;

  OPCODE incdec = (expr->token() == TOK_INCREMENT)
                  ? OP_INC_I
                  : OP_DEC_I;

  if (expr->prefix()) {
    __ opcode(incdec);
    __ opcode(OP_LOAD_I);
    return ValueDest::Pri;
  }

  // Postfix is harder.
  __ opcode(OP_MOVE_ALT);   // save addr in ALT
  __ opcode(OP_LOAD_I);     // load previous value in PRI
  __ opcode(OP_XCHG);       // PRI=addr, ALT=previous value
  __ opcode(incdec);        // incdec PRI
  return ValueDest::Alt;
}

// Note: this emits the address of the index.
ValueDest
SmxCompiler::emitIndex(sema::IndexExpr* expr, ValueDest dest)
{
  ArrayType* atype = expr->base()->type()->toArray();

  int32_t const_index = -1;
  if (expr->index()->getConstantInt32(&const_index)) {
    if (!emit_into(expr->base(), ValueDest::Pri))
      return ValueDest::Error;

    if (const_index != 0) {
      if (atype->isCharArray())
        __ opcode(OP_ADD_C, const_index);
      else
        __ opcode(OP_ADD_C, const_index * sizeof(cell_t));
    }
  } else {
    if (!emit_into(expr->base(), ValueDest::Alt))
      return ValueDest::Error;

    uint64_t saved_alt = preserve(ValueDest::Alt);
    if (!emit_into(expr->index(), ValueDest::Pri))
      return ValueDest::Error;
    restore(saved_alt);

    if (atype->hasFixedLength())
      __ opcode(OP_BOUNDS, atype->fixedLength() - 1);

    if (atype->isCharArray())
      __ opcode(OP_ADD);
    else
      __ opcode(OP_IDXADDR);
  }

  // See the comment in SemanticAnalysis::visitIndex. We use indirection
  // vectors and must force a load.
  Type* contained = atype->contained();
  if (contained->isArray() && contained->toArray()->hasFixedLength())
    __ opcode(OP_LOAD_I);

  return ValueDest::Pri;
}

ValueDest
SmxCompiler::emitVar(sema::VarExpr* expr, ValueDest dest)
{
  VariableSymbol* sym = expr->sym();
  Type* type = sym->type();

  // We should never be returning the addresses of dynamic arrays or references
  // yet.
  if (type->isArray())
    assert(type->toArray()->hasFixedLength());

  will_kill(dest);

  switch (sym->storage()) {
    case StorageClass::Argument:
    case StorageClass::Local:
      if (type->isReference()) {
        if (dest == ValueDest::Pri)
          __ opcode(OP_LOAD_S_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_LOAD_S_ALT, sym->address());
        else if (dest == ValueDest::Stack)
          __ opcode(OP_PUSH_S, sym->address());
      } else {
        if (dest == ValueDest::Pri)
          __ opcode(OP_ADDR_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_ADDR_ALT, sym->address());
        else if (dest == ValueDest::Stack)
          __ opcode(OP_PUSH_ADR, sym->address());
      }
      return dest;

    case StorageClass::Global:
      assert(!type->isReference());
      if (dest == ValueDest::Pri)
        __ const_pri(sym->address());
      else if (dest == ValueDest::Alt)
        __ const_alt(sym->address());
      else if (dest == ValueDest::Stack)
        __ opcode(OP_PUSH_C, sym->address());
      return dest;

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "emit-var-sc-kind" << int32_t(sym->storage());
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emit_var_load(sema::VarExpr* expr, ValueDest dest)
{
  VariableSymbol* sym = expr->sym();
  Type* type = sym->type();

  if (type->isArray())
    assert(!type->toArray()->hasFixedLength());

  // There is no PUSHREF_S opcode, so we may have to use PRI as a temp.
  if (type->isReference() && dest == ValueDest::Stack)
    dest = ValueDest::Pri;

  will_kill(dest);

  switch (sym->storage()) {
    case StorageClass::Argument:
    case StorageClass::Local:
      if (type->isReference()) {
        if (dest == ValueDest::Pri)
          __ opcode(OP_LREF_S_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_LREF_S_ALT, sym->address());
      } else {
        if (dest == ValueDest::Pri)
          __ opcode(OP_LOAD_S_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_LOAD_S_ALT, sym->address());
        else if (dest == ValueDest::Stack)
          __ opcode(OP_PUSH_S, sym->address());
      }
      return dest;

    case StorageClass::Global:
      assert(!type->isReference());
      if (dest == ValueDest::Pri)
        __ opcode(OP_LOAD_PRI, sym->address());
      else if (dest == ValueDest::Alt)
        __ opcode(OP_LOAD_ALT, sym->address());
      else if (dest == ValueDest::Stack)
        __ opcode(OP_PUSH, sym->address());
      else
        assert(false);
      return dest;

    default:
      cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
        "emit-var-sc-kind" << int32_t(sym->storage());
      return ValueDest::Error;
  }
}

void
SmxCompiler::emit_var_store(VariableSymbol* sym, ValueDest src)
{
  switch (sym->storage()) {
    case StorageClass::Argument:
    case StorageClass::Local:
      if (sym->type()->isReference()) {
        if (src == ValueDest::Pri)
          __ opcode(OP_SREF_S_PRI, sym->address());
        else
          __ opcode(OP_SREF_S_ALT, sym->address());
      } else {
        if (src == ValueDest::Pri)
          __ opcode(OP_STOR_S_PRI, sym->address());
        else
          __ opcode(OP_STOR_S_ALT, sym->address());
      }
      return;

    case StorageClass::Global:
      if (src == ValueDest::Pri)
        __ opcode(OP_STOR_PRI, sym->address());
      else
        __ opcode(OP_STOR_ALT, sym->address());
      return;

    default:
      assert(false);
  }
}

void
SmxCompiler::initialize_array(VariableSymbol* sym, sema::Expr* expr, const ArrayInfo& info)
{
  // Make sure we have enough space in DAT.
  if (!ke::IsUint64AddSafe(data_.size(), info.bytes) ||
      (uint64_t(data_.size()) + uint64_t(info.bytes)) > INT_MAX)
  {
    cc_.report(sym->node()->loc(), rmsg::array_too_big);
    return;
  }

  int32_t base = int32_t(data_.position());
  if (!data_.preallocate(info.bytes)) {
    cc_.report(sym->node()->loc(), rmsg::outofmemory);
    return;
  }

  // Fill with zeroes to make gen_array_data simpler.
  memset(data_.ptr<char>(base), 0, info.bytes);

  // :TODO: static.
  bool is_global = (sym->storage() == StorageClass::Global);

  ArrayBuilder builder;
  builder.info = &info;
  builder.base_delta = (is_global ? 0 : base);
  builder.iv_cursor = base;
  builder.data_cursor = base + info.iv_size;

  gen_array_iv(info.base_type, expr, builder);

  // If we generated everything correctly, we should have filled up the IV
  // space as well as the data space.
  assert(builder.iv_cursor == base + info.iv_size);
  assert(builder.data_cursor == base + info.bytes);

  // Nothing more needed for global/static arrays.
  if (is_global)
    return;

  // For local arrays, we need to copy over data and rebase the iv section.
  if (info.iv_size) {
    __ opcode(OP_ADDR_PRI, sym->address());
    __ opcode(OP_REBASE, base, info.iv_size, info.data_size);
  } else {
    __ opcode(OP_ADDR_ALT, sym->address());
    __ const_pri(base);
    __ opcode(OP_MOVS, info.bytes);
  }
}

// This function computes a template for an N-dimensional array. Let's take a
// look at two examples:
//
//    int b[2][3] = { ... };
//
// This array should have the following cells:
//
//    &b[0][0]  &b[1][0]  b[0][0]  b[0][1]  b[0][2]
//
//     b[0][3]   b[1][0]  b[1][1]  b[1][2]
//
// The first two cells are an "indirection vector" - an array of pointers to
// successive arrays. Without this model, it would be impossible to pass a
// fixed-length multi-dimensional array into a dynamically sized array type.
//
// Another example:
//    int b[2][3][4];
//
// Will contain:
//
//    &b[0][0]     &b[1][0]     &b[0][0][0]  &b[0][1][0]
//
//    &b[0][2][0]  &b[1][0][0]  &b[1][1][0]  &b[1][2][0]
//
//     b[0][0][0]   b[0][0][1]   b[0][0][2]   b[0][0][3]
//
// Etc. In this case, |b| contains a 2-entry indirection vector, and each
// array it points to has a 3-entry indirection vector pointing to the final
// dimension.
//
// We place all indirection vectors next to each other to simplify rebasing
// the template data at runtime. It is, however, possible to interleave the
// data. We don't specify how it must be constructed.
//
// The original Pawn did not require rebasing the template data. Instead,
// each address was relative to the base of the vector. We move away from that
// in SP2 for a few reasons. It's complicated and obscure, it does not reflect
// how multi-dimensional arrays work in other languages, and it is unclear how
// to make dynamic arrays and slices work in such a model.
cell_t
SmxCompiler::gen_array_iv(ArrayType* type, sema::Expr* expr, ArrayBuilder& b)
{
  Type* contained = type->contained();
  if (!contained->isArray())
    return gen_array_data(type, expr, b);

  // This case is currently not possible, syntactically, because we will take
  // the dynamic genarray path.
  ArrayType* child = contained->toArray();
  if (!child->hasFixedLength())
    return gen_array_data(type, expr, b);

  // We're an outer dimension in a multi-dimensional array. Reserve space for
  // our indirection vector.
  int32_t iv_addr = b.iv_cursor;
  b.iv_cursor += type->fixedLength() * sizeof(cell_t);

  sema::ArrayInitExpr* init = expr ? expr->toArrayInitExpr() : nullptr;
  for (int32_t i = 0; i < type->fixedLength(); i++) {
    sema::Expr* child_init = init && size_t(i) < init->exprs()->size()
                             ? init->exprs()->at(i)
                             : nullptr;
    int32_t next_array = gen_array_iv(child, child_init, b);

    int32_t iv_entry_addr = iv_addr + (i * sizeof(cell_t));
    *data_.ptr<cell_t>(iv_entry_addr) = next_array - b.base_delta;
  }
  return iv_addr;
}

cell_t
SmxCompiler::gen_array_data(ArrayType* type, sema::Expr* expr, ArrayBuilder& b)
{
  int32_t data_addr = b.data_cursor;
  b.data_cursor += b.info->data_width;

  // If there's no expr, we're done... we pre-filled everything to 0 already.
  if (!expr)
    return data_addr;

  Type* contained = type->contained();
  if (contained->isPrimitive(PrimitiveType::Char)) {
    char* ptr = data_.ptr<char>(data_addr);

    if (expr) {
      if (sema::StringExpr* lit = expr->toStringExpr()) {
        ke::SafeStrcpy(ptr, b.info->data_width, lit->literal()->chars());
      } else {
        // :TODO: test  = {'a', 'b', 'c', 0} ....
        assert(false);
      }
    }
    return data_addr;
  }

  sema::ArrayInitExpr* lit = expr->toArrayInitExpr();

  assert(lit->exprs()->size() <= size_t(type->fixedLength()));
  cell_t prev2 = 0, prev1 = 0;
  for (size_t i = 0; i < lit->exprs()->size(); i++) {
    sema::Expr* ev = lit->exprs()->at(i);
    BoxedValue box;
    if (!ev->getBoxedValue(&box)) {
      cc_.report(ev->src()->loc(), rmsg::unimpl_kind) <<
        "smx-gen-array-data" << ev->prettyName();
      continue;
    }
    prev2 = prev1;
    prev1 = GetCellFromBox(box);
    *data_.ptr<cell_t>(data_addr + i * sizeof(cell_t)) = prev1;
  }

  // 1, 2, 3 ... should yield 1, 2, 3, 4, 5 etc
  cell_t step = prev1 - prev2;
  if (!contained->isPrimitive(PrimitiveType::Int32))
    step = 0;
  for (int32_t i = int32_t(lit->exprs()->size()); i < type->fixedLength(); i++) {
    prev1 += step;
    *data_.ptr<cell_t>(data_addr + i * sizeof(cell_t)) = prev1;
  }
  return data_addr;
}

void
SmxCompiler::initialize_dynamic_array(ast::VarDecl* decl, sema::Expr* expr)
{
  // We must have an expression here - SemanticAnalysis should have guaranteed it.
  assert(expr);

  if (sema::StringExpr* str = expr->asStringExpr()) {
    Atom* literal = str->literal();
    int32_t arrayLength = CellLengthOfString(literal->length());
    int32_t data = generateDataString(literal->chars(), literal->length());

    __ opcode(OP_PUSH_C, arrayLength);
    __ opcode(OP_GENARRAY, 1);
    __ opcode(OP_POP_ALT);
    __ const_pri(data);
    __ opcode(OP_MOVS, arrayLength * sizeof(cell_t));
  } else {
    sema::NewArrayExpr* ctor = expr->toNewArrayExpr();
    for (size_t i = 0; i < ctor->exprs()->size(); i++) {
      sema::Expr* dim = ctor->exprs()->at(i);
      if (!emit_into(dim, ValueDest::Stack))
        return;
    }

    int32_t argc = int32_t(ctor->exprs()->size());
    if (decl->must_zero_init())
      __ opcode(OP_GENARRAY_Z, argc);
    else
      __ opcode(OP_GENARRAY, argc);
    __ opcode(OP_POP_ALT);
  }

  // Make sure the variable gets released when we leave scope.
  scope_info_->heap_vars++;

  emit_var_store(decl->sym(), ValueDest::Alt);
}

void
SmxCompiler::leave_scope(ScopeInfo& scope_info)
{
  // Don't generate heap pops for the top-level scope, since it will be killed
  // on function return.
  if (!scope_info.prev())
    return;

  for (size_t i = 0; i < scope_info.heap_vars; i++)
    __ opcode(OP_TRACKER_POP_SETHEAP);
}

void
SmxCompiler::jump_to_scope(ScopeInfo* stop_at)
{
  for (ScopeInfo* iter = scope_info_; iter != stop_at; iter = iter->prev())
    leave_scope(*iter);
}

static inline OPCODE
BinaryOpHasInlineTest(TokenKind kind)
{
  switch (kind) {
    case TOK_EQUALS: return OP_JEQ;
    case TOK_NOTEQUALS: return OP_JNEQ;
    case TOK_GT: return OP_JSGRTR;
    case TOK_GE: return OP_JSGEQ;
    case TOK_LT: return OP_JSLESS;
    case TOK_LE: return OP_JSLEQ;
    default: return OP_NOP;
  }
}

static inline OPCODE
InvertTestOp(OPCODE op)
{
  switch (op) {
    case OP_JEQ: return OP_JNEQ;
    case OP_JNEQ: return OP_JEQ;
    case OP_JSGRTR: return OP_JSLEQ;
    case OP_JSGEQ: return OP_JSLESS;
    case OP_JSLESS: return OP_JSGEQ;
    case OP_JSLEQ: return OP_JSGRTR;
    default:
      assert(false);
      return OP_NOP;
  }
}

void
SmxCompiler::test(sema::Expr* expr, bool jumpOnTrue, Label* taken, Label* fallthrough)
{
  if (sema::BinaryExpr* bin = expr->asBinaryExpr()) {
    // Optimize comparators into their jump-conditional instructions, to avoid
    // needing intermediate values.
    OPCODE op = BinaryOpHasInlineTest(bin->token());
    if (op != OP_NOP) {
      if (!jumpOnTrue)
        op = InvertTestOp(op);

      load_both(bin->left(), bin->right());
      __ opcode(op, taken);
      return;
    }

    // Optimize || and && so that no intermediate storage is needed.
    if (bin->token() == TOK_OR || bin->token() == TOK_AND) {
      test_logical(bin, jumpOnTrue, taken, fallthrough);
      return;
    }
  }

  if (sema::UnaryExpr* unary = expr->asUnaryExpr()) {
    // Optimize ! away.
    if (unary->token() == TOK_NOT) {
      // Note: we re-enter test so we can peel away more operations underneath the !
      test(unary->expr(), !jumpOnTrue, taken, fallthrough);
      return;
    }
  }

  // If we get here, there were no obvious shortcuts to take, so we will
  // simply emit the expression and test if it's zero.
  if (!emit_into(expr, ValueDest::Pri))
    return;

  if (jumpOnTrue)
    __ opcode(OP_JNZ, taken);
  else
    __ opcode(OP_JZER, taken);
}

void
SmxCompiler::test_logical(sema::BinaryExpr* bin, bool jumpOnTrue, Label* taken, Label* fallthrough)
{
  TokenKind token = bin->token();
  std::vector<sema::Expr*> sequence = flatten(bin);
  assert(sequence.size() >= 2);

  // a || b || c .... given jumpOnTrue, should be:
  //
  //   resolve a
  //   jtrue TAKEN
  //   resolve b
  //   jtrue TAKEN
  //   resolve c
  //   jtrue TAKEN
  //
  // a || b || c .... given jumpOnFalse, should be:
  //   resolve a
  //   jtrue FALLTHROUGH
  //   resolve b
  //   jtrue FALLTHROUGH
  //   resolve c
  //   jfalse TAKEN
  //  FALLTHROUGH:
  //
  // a && b && c ..... given jumpOnTrue, should be:
  //   resolve a
  //   jfalse FALLTHROUGH
  //   resolve b
  //   jfalse FALLTHROUGH
  //   resolve c
  //   jtrue TAKEN
  //  FALLTHROUGH:
  //
  // a && b && c ..... given jumpOnFalse, should be:
  //   resolve a
  //   jfalse TAKEN
  //   resolve b
  //   jfalse TAKEN
  //   resolve c
  //   jfalse TAKEN
  //
  // This is fairly efficient, and by re-entering test() we can ensure each
  // jfalse/jtrue encodes things like "a > b" with a combined jump+compare
  // instruction.
  //
  // Note: to make this slightly easier to read, we make all this logic
  // explicit below rather than collapsing it into a single test() call.
  for (size_t i = 0; i < sequence.size() - 1; i++) {
    sema::Expr* expr = sequence[i];
    if (token == TOK_OR) {
      if (jumpOnTrue)
        test(expr, true, taken, fallthrough);
      else
        test(expr, true, fallthrough, taken);
    } else {
      if (jumpOnTrue)
        test(expr, false, fallthrough, taken);
      else
        test(expr, false, taken, fallthrough);
    }
  }

  sema::Expr* final_expr = sequence.back();
  test(final_expr, jumpOnTrue, taken, fallthrough);
}

static inline void
flatten_recursive(sema::Expr* expr, TokenKind token, std::vector<sema::Expr*>* out)
{
  sema::BinaryExpr* bin = expr->asBinaryExpr();
  if (bin && bin->token() == token) {
    flatten_recursive(bin->left(), token, out);
    flatten_recursive(bin->right(), token, out);
  } else {
    out->push_back(expr);
  }
}

std::vector<sema::Expr*>
SmxCompiler::flatten(sema::BinaryExpr* expr)
{
  std::vector<sema::Expr*> out;
  flatten_recursive(expr->left(), expr->token(), &out);
  flatten_recursive(expr->right(), expr->token(), &out);
  return out;
}

bool
SmxCompiler::load_both(sema::Expr* left, sema::Expr* right)
{
  if (!emit_into(left, ValueDest::Pri))
    return false;

  uint64_t saved_pri = preserve(ValueDest::Pri);
  if (!emit_into(right, ValueDest::Alt))
    return false;

  restore(saved_pri);
  return true;
}

void
SmxCompiler::emit_const(ValueDest dest, cell_t value)
{
  will_kill(dest);

  switch (dest) {
    case ValueDest::Pri:
      __ const_pri(value);
      break;
    case ValueDest::Alt:
      __ const_alt(value);
      break;
    case ValueDest::Stack:
      __ opcode(OP_PUSH_C, value);
      break;
    default:
      assert(false);
  }
}

void
SmxCompiler::will_kill(ValueDest dest)
{
  if (dest == ValueDest::Stack)
    return;

  assert(dest == ValueDest::Pri || dest == ValueDest::Alt);

  uint64_t* slot = nullptr;
  OPCODE op = OP_NOP;
  if (dest == ValueDest::Pri) {
    slot = &pri_value_;
    op = OP_PUSH_PRI;
  } else {
    slot = &alt_value_;
    op = OP_PUSH_ALT;
  }

  // We don't bother asserting that the slot is a particular value. We just
  // push it. If stuff is unbalanced, it will be caught in emitStatement or in
  // restore().
  if (*slot)
    __ opcode(op);

  *slot = 0;
}

uint64_t
SmxCompiler::preserve(ValueDest dest)
{
  assert(dest == ValueDest::Pri || dest == ValueDest::Alt);

  SValue value(dest);
  operand_stack_.push_back(value);

  uint64_t* slot = (dest == ValueDest::Pri)
                   ? &pri_value_
                   : &alt_value_;
  if (*slot) {
    cc_.report(SourceLocation(), rmsg::regalloc_error) <<
      "saving register without a clobber";
  }

  *slot = value.id();
  assert(pri_value_ != alt_value_);

  return value.id();
}

// Restore a register that was previously saved.
void
SmxCompiler::restore(uint64_t id)
{
  if (operand_stack_.empty() || operand_stack_.back().id() != id) {
    cc_.report(SourceLocation(), rmsg::regalloc_error) <<
      "restored register is not top of operand stack";
    return;
  }

  SValue value = ke::PopBack(&operand_stack_);
  assert(value.where() == ValueDest::Pri || value.where() == ValueDest::Alt);

  uint64_t* slot = nullptr;
  OPCODE op = OP_NOP;
  if (value.where() == ValueDest::Pri) {
    slot = &pri_value_;
    op = OP_POP_PRI;
  } else {
    slot = &alt_value_;
    op = OP_POP_ALT;
  }

  if (*slot == value.id()) {
    // The value hasn't been changed or killed, so we can just clear it.
    *slot = 0;
    return;
  }

  // If another value is occupying this register, it means we forgot to kill
  // it somewhere. Or we have an antipattern like:
  //    save pri -> A
  //    kill pri
  //    save pri -> B
  //    restore A
  //
  // But this should have been caught above via the operand stack.
  if (*slot != 0) {
    cc_.report(SourceLocation(), rmsg::regalloc_error) <<
      "restoring saved register would overwrite another value";
    return;
  }

  __ opcode(op);
  *slot = 0;
}

static bool
HasSimpleCellStorage(Type* type)
{
  switch (type->canonicalKind()) {
    case Type::Kind::Primitive:
    case Type::Kind::Enum:
    case Type::Kind::Unchecked:
    case Type::Kind::Function:
    case Type::Kind::MetaFunction:
      return true;
    case Type::Kind::Array:
      return !type->toArray()->hasFixedLength();
    default:
      return false;
  }
}

int32_t
SmxCompiler::compute_storage_size(Type* type)
{
  if (HasSimpleCellStorage(type))
    return sizeof(cell_t);

  assert(false);

  cc_.report(SourceLocation(), rmsg::unimpl_kind) <<
    "smx-storage-size" << int32_t(type->canonicalKind());
  return 10000000;
}

int
SmxCompiler::sort_functions(const void* a1, const void* a2)
{
  FunctionEntry& f1 = *(FunctionEntry*)a1;
  FunctionEntry& f2 = *(FunctionEntry*)a2;
  return strcmp(f1.name->chars(), f2.name->chars());
}

static cell_t
GetCellFromBox(const BoxedValue& box)
{
  if (box.isBool())
    return box.toBool() ? 1 : 0;
  if (box.isInteger()) {
    const IntValue& iv = box.toInteger();
    assert(iv.valueFitsInInt32());
    return (int32_t)iv.asSigned();
  }
  assert(false);
  return 0;
}

} // namespace sp
