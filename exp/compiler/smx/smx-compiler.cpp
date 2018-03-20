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
#include "compile-context.h"
#include "parser/ast.h"
#include "scopes.h"
#include "smx-compiler.h"
#include <amtl/am-algorithm.h>
#include <amtl/am-maybe.h>
#include <smx/smx-v1.h>
#include <sp_vm_types.h>

#define __ masm_.

namespace sp {

static bool HasSimpleCellStorage(Type* type);

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
   continue_to_(nullptr),
   break_to_(nullptr),
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
  code->header().codeversion = 12;
  code->header().flags = CODEFLAG_DEBUG;
  code->header().main = 0;
  code->header().code = sizeof(sp_file_code_t);
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
  if (natives_.length())
    qsort(natives_.buffer(), natives_.length(), sizeof(FunctionEntry), sort_functions);

  RefPtr<SmxNativeSection> natives =  new SmxNativeSection(".natives");
  for (size_t i = 0; i < natives_.length(); i++) {
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
  assert(publics_.length());
  qsort(publics_.buffer(), publics_.length(), sizeof(FunctionEntry), sort_functions);

  RefPtr<SmxPublicSection> publics = new SmxPublicSection(".publics");
  for (size_t i = 0; i < publics_.length(); i++) {
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
  qsort(program_->globals.buffer(), program_->globals.length(), sizeof(ast::VarDecl*), sort_vars);

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
  if (params->length() >= kMaxArgSlots)
    cc_.report(fun->loc(), rmsg::too_many_arguments);

  for (size_t i = 0; i < params->length(); i++) {
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
    ke::UniquePtr<char[]> decorated(new char[length]);
    ke::SafeSprintf(decorated.get(), length, "%s%s", decorated_prefix, name->chars());
    name = cc_.add(decorated.get(), length);
  }

  publics_.append(FunctionEntry(name, fun));
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

  // :TODO: track stack depth
  switch (stmt->kind()) {
    case ast::AstKind::kReturnStatement:
      generateReturn(stmt->toReturnStatement());
      break;
    case ast::AstKind::kExpressionStatement:
      generateExprStatement(stmt->toExpressionStatement());
      break;
    case ast::AstKind::kVarDecl:
      generateVarDecl(stmt->toVarDecl());
      break;
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
}

void
SmxCompiler::generateBlock(ast::BlockStatement* block)
{
  // :TODO: raii this, so for loops can use it for the init scope
  int32_t stk_usage = cur_var_stk_;

  ast::StatementList* statements = block->statements();
  for (size_t i = 0; i < statements->length(); i++) {
    ast::Statement* stmt = statements->at(i);
    generateStatement(stmt);
  }

  // Commit this scope's stack usage to the frame, then drop our local usage.
  max_var_stk_ = ke::Max(max_var_stk_, cur_var_stk_);
  cur_var_stk_ = stk_usage;
}

void
SmxCompiler::generateVarDecl(ast::VarDecl* stmt)
{
  // :TODO: assert not const
  VariableSymbol* sym = stmt->sym();
  int32_t size = compute_storage_size(sym->type());

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

  if (sema::Expr* init = stmt->sema_init()) {
    if (!HasSimpleCellStorage(sym->type())) {
      cc_.report(stmt->loc(), rmsg::unimpl_kind) <<
        "var-decl-init", BuildTypeName(sym->type());
      return;
    }

    store_into(sym, init);
  } else {
    // assert(false);
    // :TODO: store 0
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
    __ opcode(OP_CONST_PRI, 0);
  __ opcode(OP_RETN);
}

void
SmxCompiler::generateWhile(ast::WhileStatement* stmt)
{
  sema::Expr* cond = stmt->sema_cond();

  Label taken, fallthrough;
  if (stmt->token() == TOK_WHILE) {
    ke::SaveAndSet<Label*> save_break(&break_to_, &taken);
    ke::SaveAndSet<Label*> save_continue(&continue_to_, &fallthrough);

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
    ke::SaveAndSet<Label*> save_break(&break_to_, &taken);
    ke::SaveAndSet<Label*> save_continue(&continue_to_, &fallthrough);

    // repeat:
    //
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
    ke::SaveAndSet<Label*> save_break(&break_to_, &done);
    ke::SaveAndSet<Label*> save_continue(&continue_to_, &update);

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

  for (size_t i = 0; i < stmt->clauses()->length(); i++) {
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
  __ opcode(OP_JUMP, break_to_);
}

void
SmxCompiler::generateSwitch(ast::SwitchStatement* stmt)
{
  PoolList<ast::Case*>* cases = stmt->cases();

  size_t total_cases = 0;
  for (size_t i = 0; i < cases->length(); i++) {
    ast::Case* entry = cases->at(i);
    total_cases += entry->values()->length();
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

  ke::UniquePtr<Label[]> labels(new Label[cases->length()]);

  // Generate entries for the CASETBL opcode above.
  for (size_t i = 0; i < cases->length(); i++) {
    ast::Case* entry = cases->at(i);

    for (size_t j = 0; j < entry->values()->length(); j++) {
      cell_t value = entry->values()->at(j);
      __ casetbl_entry(value, &labels[i]);
    }
  }

  Label done;

  // Now generate the actual switch cases.
  for (size_t i = 0; i < cases->length(); i++) {
    ast::Case* entry = cases->at(i);

    __ bind(&labels[i]);
    generateStatement(entry->statement());

    // All but the very last case should jump past the other cases.
    if (i != cases->length() - 1 || stmt->defaultCase())
      __ opcode(OP_JUMP, &done);
  }

  __ bind(&defcase);
  if (ast::Statement* defaultCase = stmt->defaultCase())
    generateStatement(defaultCase);
  __ bind(&done);
}

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
SmxCompiler::emit(sema::Expr* expr, ValueDest dest)
{
  switch (expr->kind()) {
  case sema::ExprKind::ConstValue:
    return emitConstValue(expr->toConstValueExpr(), dest);
  case sema::ExprKind::Binary:
    return emitBinary(expr->toBinaryExpr(), dest);
  case sema::ExprKind::Unary:
    return emitUnary(expr->toUnaryExpr(), dest);
  case sema::ExprKind::Call:
    return emitCall(expr->toCallExpr(), dest);
  case sema::ExprKind::Var:
    return emitVar(expr->toVarExpr(), dest);
  case sema::ExprKind::TrivialCast:
    return emitTrivialCast(expr->toTrivialCastExpr(), dest);
  case sema::ExprKind::String:
    return emitString(expr->toStringExpr(), dest);
  case sema::ExprKind::IncDec:
    return emitIncDec(expr->toIncDecExpr(), dest);
  case sema::ExprKind::Index:
    return emitIndex(expr->toIndexExpr(), dest);
  default:
    cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
      "smx-emit-expr" << expr->prettyName();
    return ValueDest::Error;
  }
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
SmxCompiler::emitVar(sema::VarExpr* expr, ValueDest dest)
{
  VariableSymbol* sym = expr->sym();

  if (sym->type()->isArray()) {
    switch (sym->storage()) {
      case StorageClass::Argument:
      case StorageClass::Local:
        will_kill(dest);
        if (dest == ValueDest::Pri)
          __ opcode(OP_ADDR_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_ADDR_ALT, sym->address());
        else if (dest == ValueDest::Stack)
          __ opcode(OP_PUSH_ADR, sym->address());
        else
          assert(false);
        return dest;

      case StorageClass::Global:
        will_kill(dest);
        if (dest == ValueDest::Pri)
          __ opcode(OP_CONST_PRI, sym->address());
        else if (dest == ValueDest::Alt)
          __ opcode(OP_CONST_ALT, sym->address());
        else if (dest == ValueDest::Stack)
          __ opcode(OP_PUSH_C, sym->address());
        else
          assert(false);
        return dest;

      default:
        cc_.report(expr->src()->loc(), rmsg::unimpl_kind) <<
          "emit-arr-sc-kind" << int32_t(sym->storage());
        return ValueDest::Error;
    }
  }

  assert(sym->type()->isPrimitive());

  // :TODO: inline these
  assert(!sym->isConstExpr());

  switch (sym->storage()) {
    case StorageClass::Argument:
    case StorageClass::Local:
      will_kill(dest);
      if (dest == ValueDest::Pri)
        __ opcode(OP_LOAD_S_PRI, sym->address());
      else if (dest == ValueDest::Alt)
        __ opcode(OP_LOAD_S_ALT, sym->address());
      else if (dest == ValueDest::Stack)
        __ opcode(OP_PUSH_S, sym->address());
      else
        assert(false);
      return dest;

    case StorageClass::Global:
      will_kill(dest);
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
        ke::Swap(left, right);
        ke::Swap(left_i32, right_i32);
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

      uint64_t saved_pri = save(ValueDest::Pri);
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
          __ opcode(OP_SDIV);
          break;
        default:
          assert(false);
          break;
      }
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
SmxCompiler::emitTrivialCast(sema::TrivialCastExpr* expr, ValueDest dest)
{
  // Pass-through - we don't generate code for trivial casts, since the
  // bytecode is not typed.
  return emit(expr->expr(), dest);
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
  if (args->length() > kMaxArgs)
    cc_.report(expr->src()->loc(), rmsg::too_many_arguments);

  for (size_t i = args->length() - 1; i < args->length(); i--) {
    sema::Expr* expr = args->at(i);

    size_t opstack_size = operand_stack_.length();

    emit_into(expr, ValueDest::Stack);

    // Make sure emit_into does not cause any spills (or, if it did, that the
    // spills were cleaned up internally). Otherwise the stack will be
    // misaligned.
    if (opstack_size != operand_stack_.length()) {
      cc_.report(SourceLocation(), rmsg::regalloc_error) <<
        "argument pushed too many values onto the stack";
    }
  }

  if (sig->native()) {
    // Mark the native as used.
    if (!fun->impl()->address()->used())
      natives_.append(FunctionEntry(fun->name(), fun->impl()));

    __ sysreq_n(fun->impl()->address(), (uint32_t)args->length());
    assert(fun->impl()->address()->used());
  } else {
    __ opcode(OP_PUSH_C, cell_t(args->length()));
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

  int32_t address = int32_t(data_.pos());
  sym->allocate(StorageClass::Global, address);

  data_.write<cell_t>(0);

  sema::Expr* init = decl->sema_init();
  if (!init)
    return;

  uint8_t* bytes = data_.bytes() + sym->address();
  switch (init->kind()) {
    case sema::ExprKind::ConstValue:
    {
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

  Vector<int32_t> values;

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
    if (sym->type()->isString()) {
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

    values.append(value);
    field_index++;
  }

  int32_t address = int32_t(data_.pos());
  sym->allocate(StorageClass::Global, address);

  for (int32_t value : values)
    data_.write<cell_t>(value);
}

ValueDest
SmxCompiler::emitString(sema::StringExpr* expr, ValueDest dest)
{
  Atom* literal = expr->literal();

  // The array size should be bytes + 1, for the null terminator.
  assert(expr->type()->toArray()->fixedLength() == literal->length() + 1);

  int32_t address = generateDataString(literal->chars(), literal->length());
  emit_const(dest, address);
  return dest;
}

int32_t
SmxCompiler::generateDataString(const char* str, size_t length)
{
  int32_t address = int32_t(data_.pos());

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
  data_.write(str, length);
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
  sema::Expr* lvalue = expr->expr();

  will_kill(dest);

  switch (lvalue->kind()) {
    case sema::ExprKind::Var:
    {
      sema::VarExpr* var = lvalue->toVarExpr();
      VariableSymbol* sym = var->sym();
      assert(!sym->type()->isReference());

      if (expr->prefix())
        emitVar(var, dest);

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

      if (expr->postfix())
        emitVar(var, dest);
      return dest;
    }
    default:
      cc_.report(lvalue->src()->loc(), rmsg::unimpl_kind) <<
        "smx-emit-incdec" << lvalue->prettyName();
      return ValueDest::Error;
  }
}

ValueDest
SmxCompiler::emitIndex(sema::IndexExpr* expr, ValueDest dest)
{
  ArrayType* atype = expr->base()->type()->toArray();

  int32_t const_index = -1;
  if (expr->index()->getConstantInt32(&const_index)) {
    if (!emit_into(expr->base(), ValueDest::Pri))
      return ValueDest::Error;

    if (atype->isString())
      __ opcode(OP_ADD_C, const_index);
    else
      __ opcode(OP_ADD_C, const_index * sizeof(cell_t));
  } else {
    if (!emit_into(expr->base(), ValueDest::Alt))
      return ValueDest::Error;

    uint64_t saved_alt = save(ValueDest::Alt);
    if (!emit_into(expr->index(), ValueDest::Pri))
      return ValueDest::Error;
    restore(saved_alt);

    if (atype->hasFixedLength())
      __ opcode(OP_BOUNDS, atype->fixedLength() - 1);

    if (atype->isString())
      __ opcode(OP_ADD);
    else
      __ opcode(OP_IDXADDR);
  }

  if (atype->isString())
    __ opcode(OP_LODB_I);
  else
    __ opcode(OP_LOAD_I);
  return ValueDest::Pri;
}

static inline OPCODE
BinaryOpHasInlineTest(TokenKind kind)
{
  switch (kind) {
    case TOK_EQUALS:
      return OP_JEQ;
    case TOK_NOTEQUALS:
      return OP_JNEQ;
    case TOK_GT:
      return OP_JSGRTR;
    case TOK_GE:
      return OP_JSGEQ;
    case TOK_LT:
      return OP_JSLESS;
    case TOK_LE:
      return OP_JSLEQ;
    default:
      return OP_NOP;
  }
}

static inline OPCODE
InvertTestOp(OPCODE op)
{
  switch (op) {
    case OP_JEQ:
      return OP_JNEQ;
    case OP_JNEQ:
      return OP_JEQ;
    case OP_JSGRTR:
      return OP_JSLEQ;
    case OP_JSGEQ:
      return OP_JSLESS;
    case OP_JSLESS:
      return OP_JSGEQ;
    case OP_JSLEQ:
      return OP_JSGRTR;
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
  ke::Vector<sema::Expr*> sequence = flatten(bin);
  assert(sequence.length() >= 2);

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
  for (size_t i = 0; i < sequence.length() - 1; i++) {
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
flatten_recursive(sema::Expr* expr, TokenKind token, ke::Vector<sema::Expr*>* out)
{
  sema::BinaryExpr* bin = expr->asBinaryExpr();
  if (bin && bin->token() == token) {
    flatten_recursive(bin->left(), token, out);
    flatten_recursive(bin->right(), token, out);
  } else {
    out->append(expr);
  }
}

ke::Vector<sema::Expr*>
SmxCompiler::flatten(sema::BinaryExpr* expr)
{
  ke::Vector<sema::Expr*> out;
  flatten_recursive(expr->left(), expr->token(), &out);
  flatten_recursive(expr->right(), expr->token(), &out);
  return out;
}

bool
SmxCompiler::load_both(sema::Expr* left, sema::Expr* right)
{
  if (!emit_into(left, ValueDest::Pri))
    return false;

  uint64_t saved_pri = save(ValueDest::Pri);
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
      __ opcode(OP_CONST_PRI, value);
      break;
    case ValueDest::Alt:
      __ opcode(OP_CONST_ALT, value);
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
SmxCompiler::save(ValueDest dest)
{
  assert(dest == ValueDest::Pri || dest == ValueDest::Alt);

  SValue value(dest);
  operand_stack_.append(value);

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

  SValue value = operand_stack_.popCopy();
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
    default:
      return false;
  }
}

void
SmxCompiler::store_into(VariableSymbol* sym, sema::Expr* init)
{
  Type* type = sym->type();
  assert(HasSimpleCellStorage(type));

  switch (sym->scope()->kind()) {
    case Scope::Block:
      emit_into(init, ValueDest::Pri);
      __ opcode(OP_STOR_S_PRI, sym->address());
      break;

    default:
      cc_.report(SourceLocation(), rmsg::unimpl_kind) <<
        "store-int-scope-kind" << int32_t(sym->scope()->kind());
  }
}

int32_t
SmxCompiler::compute_storage_size(Type* type)
{
  if (HasSimpleCellStorage(type))
    return sizeof(cell_t);

  if (type->isArray()) {
    ArrayType* atype = type->toArray();
    assert(atype->hasFixedLength());

    if (atype->isString())
      return ke::Align(atype->fixedLength(), sizeof(cell_t));
    return atype->fixedLength() * sizeof(cell_t);
  }

  cc_.report(SourceLocation(), rmsg::unimpl_kind) <<
    "smx-storage-size" << int32_t(type->canonicalKind());
  return 10000000;
}

int
SmxCompiler::sort_functions(const void* a1, const void* a2)
{
  FunctionEntry& f1 = *(FunctionEntry *)a1;
  FunctionEntry& f2 = *(FunctionEntry *)a2;
  return strcmp(f1.name->chars(), f2.name->chars());
}

} // namespace sp
