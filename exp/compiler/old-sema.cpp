// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#include "semantic-analysis.h"
#include "compile-context.h"

using namespace ke;
using namespace sp;

SemanticAnalysis::SemanticAnalysis(CompileContext &cc, TranslationUnit *unit)
 : pool_(cc.pool()),
   cc_(cc),
   unit_(unit),
   scope_(unit_->globalScope()),
   fun_(nullptr),
   loop_(nullptr),
   hir_(nullptr),
   outp_(nullptr)
{
}

SemanticAnalysis::SemanticAnalysis(SemanticAnalysis *parent, FunctionStatement *node)
 : pool_(parent->pool_),
   cc_(parent->cc_),
   unit_(parent->unit_),
   scope_(parent->scope_),
   fun_(nullptr /* :TODO: node->sym()->type()*/),
   loop_(nullptr),
   hir_(nullptr),
   outp_(nullptr)
{
}

bool
SemanticAnalysis::analyze(ParseTree *tree)
{
  for (size_t i = 0; i < tree->statements()->length(); i++) {
    Statement *stmt = tree->statements()->at(i);
    stmt->accept(this);

    if (!cc_.canContinueProcessing())
      return false;
  }

  return cc_.phasePassed();
}

class AutoEnterScope
{
 public:
  AutoEnterScope(Scope **prevp, Scope *scope)
   : prevp_(prevp),
     prev_(*prevp),
     scope_(scope)
  {
    if (!scope_)
      return;
    prev_ = *prevp_;
    *prevp_ = scope;
    assert(prev_ == scope->enclosing());
  }
  ~AutoEnterScope()
  {
    if (scope_)
      *prevp_ = prev_;
  }

 private:
  Scope **prevp_;
  Scope *prev_;
  Scope *scope_;
};

bool
SemanticAnalysis::analyze(FunctionStatement *node)
{
  AutoEnterScope funScope(&scope_, node->funScope());

  emit_prologue(node);
  node->body()->accept(this);
  emit_epilogue();

  return cc_.phasePassed();
}

void
SemanticAnalysis::visitFunctionStatement(FunctionStatement *node)
{
  assert(scope_->kind() == Scope::Global);

  if (node->token() == TOK_NATIVE) {
    emit_native(node);
    return;
  }
  if (node->token() == TOK_FORWARD)
    return;

  emit_function(node);
}

void
SemanticAnalysis::visitBlockStatement(BlockStatement *node)
{
  AutoEnterScope scope(&scope_, node->scope());

  for (size_t i = 0; i < node->statements()->length(); i++) {
    Statement *stmt = node->statements()->at(i);
    stmt->accept(this);
  }
}

void
SemanticAnalysis::visitVarDecl(VarDecl *node)
{
  PoolScope hir_scope;
  begin_hir(hir_scope);

  HIR *hir = nullptr;
  if (node->initialization()) {
    if ((hir = rvalue(node->initialization())) == nullptr)
      return;
    if ((hir = coerce(hir, node->sym()->type(), Coerce_Assign)) == nullptr)
      return;
  } else {
    Type *type = node->sym()->type();
    if (type->isInt32() || type->isEnum() || type->isChar() || type->isBool()) {
      hir = new (pool_) HInteger(node, type, 0);
    } else if (type->isFloat()) {
      hir = new (pool_) HFloat(node, type, 0);
    } else {
      assert(type->isArray());
    }
  }

  emit_variable(node, hir);
}

Type *
SemanticAnalysis::coercionType(TokenKind token, HIR *left, HIR *right)
{
  Type *l = left->type();
  Type *r = right->type();

  if (l->isReference())
    l = l->toReference()->contained();
  if (r->isReference())
    r = r->toReference()->contained();

  if (token >= TOK_PLUS && token <= TOK_SLASH) {
    // We allow: [float|int32] + [float|int32]
    if (l->isFloat() && r->isInt32())
      return l;
    if (l->isInt32() && r->isFloat())
      return r;
    if (l->isInt32() && r->isInt32())
      return l;
  }

  assert(l == r);

  return l;
}

void
SemanticAnalysis::visitIntegerLiteral(IntegerLiteral *node)
{
  Type *type = cc_.types()->getPrimitive(PrimitiveType::Int32);
  hir_ = new (pool_) HInteger(node, type, node->value());
}

void
SemanticAnalysis::visitTokenLiteral(TokenLiteral *node)
{
  Type *type = cc_.types()->getPrimitive(PrimitiveType::Bool);
  hir_ = new (pool_) HBoolean(node, type, node->token());
}

static inline bool
IsNumericType(Type *type)
{
  if (!type->isPrimitive())
    return false;
  return type->primitive() == PrimitiveType::Int32 ||
         type->primitive() == PrimitiveType::Float;
}

void
SemanticAnalysis::visitUnaryExpression(UnaryExpression *expr)
{
  TokenKind tok = expr->token();
  HIR *hir = rvalue(expr->expression());
  if (!hir)
    return;

  switch (tok) {
    case TOK_NEGATE: {
      if (!IsNumericType(hir->type())) {
        cc_.report(expr->loc(), rmsg::unary_type_mismatch)
          << "-"
          << hir->type();
        return;
      }
      hir_ = new (pool_) HNegate(expr, hir);
      break;
    }

    case TOK_TILDE: {
      if (!hir->type()->isInt32OrEnum()) {
        cc_.report(expr->loc(), rmsg::unary_type_mismatch)
          << "~"
          << hir->type();
        return;
      }
      hir_ = new (pool_) HInvert(expr, hir);
      break;
    }

    case TOK_NOT: {
      if (!hir->type()->isInt32OrEnum() && !hir->type()->isBool()) {
        cc_.report(expr->loc(), rmsg::unary_type_mismatch)
          << "!"
          << hir->type();
        return;
      }
      Type *output = cc_.types()->getPrimitive(PrimitiveType::Bool);
      hir_ = new (pool_) HNot(expr, output, hir);
      break;
    }

    default:
      assert(false);
  }
}

HIR *
SemanticAnalysis::binary(AstNode *node, TokenKind token, HIR *left, HIR *right)
{
  Type *coercion = coercionType(token, left, right);
  if (!coercion)
    return nullptr;
  if ((left = coerce(left, coercion, Coerce_Arg)) == nullptr)
    return nullptr;
  if ((right = coerce(right, coercion, Coerce_Arg)) == nullptr)
    return nullptr;
  
  switch (token) {
    case TOK_PLUS:
    case TOK_MINUS:
    case TOK_STAR:
    case TOK_SLASH:
    case TOK_PERCENT:
    case TOK_AMPERSAND:
    case TOK_BITOR:
    case TOK_BITXOR:
    case TOK_SHR:
    case TOK_USHR:
    case TOK_SHL:
      return new (pool_) HBinary(node, left->type(), token, left, right);

    case TOK_EQUALS:
    case TOK_NOTEQUALS:
    case TOK_LT:
    case TOK_LE:
    case TOK_GT:
    case TOK_GE:
    case TOK_AND:
    case TOK_OR:
    {
      Type *result = cc_.types()->getPrimitive(PrimitiveType::Bool);
      return new (pool_) HBinary(node, result, token, left, right);
    }

    default:
      assert(false);
      return nullptr;
  }
}

void
SemanticAnalysis::visitTernaryExpression(TernaryExpression *node)
{
  HIR *left, *right;
  if ((left = rvalue(node->left())) == nullptr)
    return;
  if ((right = rvalue(node->right())) == nullptr)
    return;
  assert(left->type() == right->type());

  HTernary *tern = new (pool_) HTernary(node, left, right);
  visitForTest(tern->test(),
               node->condition(),
               tern->success(),
               tern->failure(),
               tern->success());

  hir_ = tern;
}

void
SemanticAnalysis::visitBinaryExpression(BinaryExpression *node)
{
  HIR *left = rvalue(node->left());
  HIR *right = rvalue(node->right());
  if (!left || !right)
    return;

  hir_ = binary(node, node->token(), left, right);
}

void
SemanticAnalysis::visitIncDecExpression(IncDecExpression *node)
{
  LValue lval;
  if (!lvalue(node->expression(), &lval))
    return;

  Type *type = lval.type();
  if (!type->isInt32() && !type->isFloat()) {
    assert(false);
    return;
  }

  if (node->postfix()) {
    hir_ = new (pool_) HPostIncDec(node, type, node->token(), lval);
  } else {
    HIR *rval;
    if (lval.type()->isInt32())
      rval = new (pool_) HInteger(node, type, 1);
    else
      rval = new (pool_) HFloat(node, type, 1.0);
    hir_ = new (pool_) HStore(node, type, node->token(), lval, rval);
  }
}

void
SemanticAnalysis::visitNameProxy(NameProxy *proxy)
{
  Symbol *sym = proxy->sym();
  VariableSymbol *var = sym->asVariable();

  // If we see that a symbol is a function literal, then we bypass the scope
  // chain operations entirely and hardcode the function literal.
  if (sym->isFunction()) {
    assert(sym->scope()->kind() == Scope::Global);
    hir_ = new (pool_) HFunction(proxy, sym->asFunction());
    return;
  }

  if (value_context_ == kLValue) {
    // Egads! We're being asked to construct an l-value instead of an r-value.
    *outp_ = LValue(var);
    return;
  }

  Scope *in = sym->scope();
  switch (in->kind()) {
    case Scope::Global:
      hir_ = new (pool_) HGlobal(proxy, var);
      return;

    case Scope::Function:
    {
      assert(var->storage() == VariableSymbol::Arg);
      // Since we're in an r-value context, we need to strip the reference type.
      Type *type = var->type();
      if (type->isReference())
        type = type->toReference()->contained();
      hir_ = new (pool_) HLocal(proxy, type, var);
      return;
    }

    default:
      assert(in->kind() == Scope::Block);
      assert(var->storage() == VariableSymbol::Local);
      hir_ = new (pool_) HLocal(proxy, var->type(), var);
      return;
  }
}

void
SemanticAnalysis::visitAssignment(Assignment *node)
{
  LValue lval;
  if (!lvalue(node->lvalue(), &lval))
    return;

  HIR *hir = rvalue(node->expression());
  if ((hir = coerce(hir, lval.type(), Coerce_Assign)) == nullptr)
    return;

  hir_ = new (pool_) HStore(node, lval.type(), node->token(), lval, hir);
}

void
SemanticAnalysis::visitIndexExpression(IndexExpression *node)
{
  HIR *left = rvalue(node->left());
  if (!left)
    return;

  HIR *right = rvalue(node->right());
  if (!right)
    return;

  if (!right->type()->isInt32OrEnum()) {
    //cc_.reportError(node->right()->loc(), Message_IndexMustBeInteger);
    return;
  }
  
  // Do not coerce here, there's no reason to yet.
  if (!left->type()->isArray()) {
    //cc_.reportError(node->left()->loc(), Message_IndexBaseMustBeArray);
    return;
  }

  if (value_context_ == kLValue) {
    *outp_ = LValue(left, right);
    return;
  }

  Type *result = left->type()->toArray()->contained();

  hir_ = new (pool_) HIndex(node, result, left, right);
}

static inline TokenKind
InvertTest(TokenKind tok)
{
  switch (tok) {
   case TOK_EQUALS:
    return TOK_NOTEQUALS;
   case TOK_NOTEQUALS:
    return TOK_EQUALS;
   case TOK_LT:
    return TOK_GE;
   case TOK_LE:
    return TOK_GT;
   case TOK_GT:
    return TOK_LE;
   case TOK_GE:
    return TOK_LT;
   default:
    assert(false);
    return TOK_NONE;
  }
}

// visitForTest helps optimize statements with expressions like:
//    if (x && y || z)
//
// Where, with a naive conversion we would reduce (x && y) to a boolean, and
// then reduce (that || z) to another boolean, and another test - we can simply
// test each individual component and combine the jump paths with the paths
// the if-test needs to take.
//
// Unfortunately, this logic comes at a complexity price: it is illegal to
// emit non-statements from SemA, since the following situation could occur:
//    (1) SemA builds HIR chain #1 for statement X.
//    (2) SemA builds HIR chain #2 for statement X.
//    (3) SemA forces HIR chain #2 to be emitted to the code stream.
//    (4) SemA emits statement X, with HIR chain #1.
//
// In this situation, the HIR chains have been emitted out-of-order. So,
// visitForTest takes in a HIRList which it populates with instructions, which
// may include internal statements (like jumps and binds).
//
// The trueBranch and falseBranch parameters are self-explanatory. The
// fallthrough case describes which branch comes immediately after the
// test. This is used to optimize cases like:
//   if (x) {
//     ...
//   }
//   do {
//     ...
//   } while (y);
//
// After testing |x|, if it evaluated to true, there is no need to jump to the
// true branch, because we'll fallthrough (as long as the expression does not
// have logical ands/ors which could short-circuit). Similarly for |y|, the
// false condition does not need an actual jump target.
//
void
SemanticAnalysis::visitForTest(HIRList *output,
                               Expression *expr,
                               Label *trueBranch,
                               Label *falseBranch,
                               Label *fallthrough)
{
  Type *boolType = cc_.types()->getPrimitive(PrimitiveType::Bool);

  // Handle logical and/or.
  BinaryExpression *bin = expr->asBinaryExpression();
  if (bin && (bin->token() == TOK_AND || bin->token() == TOK_OR)) {
    HLabel *next = new (pool_) HLabel();
    if (bin->token() == TOK_AND)
      visitForTest(output, bin->left(), next, falseBranch, next);
    else
      visitForTest(output, bin->left(), trueBranch, next, next);
    output->append(new (pool_) HBind(expr, next));
    visitForTest(output, bin->right(), trueBranch, falseBranch, fallthrough);
    return;
  }

  // Handle equality and relational operators.
  if (bin && (bin->token() >= TOK_EQUALS && bin->token() <= TOK_GE)) {
    HIR *left = rvalue(bin->left());
    HIR *right = rvalue(bin->right());
    if (!left || !right)
      return;
    Type *coercion = coercionType(bin->token(), left, right);
    if (!coercion ||
        ((left = coerce(left, coercion, Coerce_Arg)) == nullptr) ||
        ((right = coerce(right, coercion, Coerce_Arg)) == nullptr))
    {
      return;
    }
    Label *target = (fallthrough == trueBranch) ? falseBranch : trueBranch;
    TokenKind token = (fallthrough == trueBranch)
                      ? InvertTest(bin->token())
                      : bin->token();
    output->append(new (pool_) HCompareAndJump(bin, token, left, right, target));
    return;
  }

  // Handle unary not (!)
  UnaryExpression *unary = expr->asUnaryExpression();
  if (unary && unary->token() == TOK_NOT) {
    // Re-invoke visitForTest but invert the branches. Note that we don't touch
    // |fallthrough|, since it's used to determine whether to jump on success
    // or failure, so inverting it as well would do nothing.
    visitForTest(output, unary->expression(), falseBranch, trueBranch, fallthrough);
    return;
  }

  // We couldn't match anything easy, so just coerce the input to a boolean.
  HIR *hir = rvalue(expr);
  if (!hir || ((hir = coerce(hir, boolType, Coerce_Assign)) == nullptr))
    return;

  if (fallthrough == falseBranch)
    output->append(new (pool_) HJump(expr, hir, true, trueBranch));
  else
    output->append(new (pool_) HJump(expr, hir, false, falseBranch));
}

void
SemanticAnalysis::visitIfStatement(IfStatement *node)
{
  PoolScope hir_scope;
  begin_hir(hir_scope);

  Label join;
  HIRList output;

  if (!node->ifFalse()) {
    Label success;

    visitForTest(&output, node->condition(), &success, &join, &success);
    emit_hirlist(&output);
    emit_bind(&success);
    node->ifTrue()->accept(this);
  } else {
    Label success, next;

    visitForTest(&output, node->condition(), &success, &next, &success);
    emit_hirlist(&output);
    emit_bind(&success);
    node->ifTrue()->accept(this);
    emit_jump(&join);
    emit_bind(&next);
    node->ifFalse()->accept(this);
  }

  emit_bind(&join);
}

void
SemanticAnalysis::visitWhileStatement(WhileStatement *node)
{
  PoolScope hir_scope;
  begin_hir(hir_scope);

  HIRList output;
  Label header, join;

  emit_bind(&header);

  LoopScope loop(scope_, &join, &header, &loop_);
  if (node->token() == TOK_WHILE) {
    Label body;
    visitForTest(&output, node->condition(), &body, &join, &body);
    emit_hirlist(&output);
    node->body()->accept(this);
    emit_jump(&header);
  } else {
    node->body()->accept(this);
    visitForTest(&output, node->condition(), &header, &join, &join);
    emit_hirlist(&output);
  }

  emit_bind(&join);
}

void
SemanticAnalysis::visitForStatement(ForStatement *node)
{
  PoolScope hir_scope;
  begin_hir(hir_scope);

  AutoEnterScope scope(&scope_, node->scope());

  Label join, update;
  LoopScope loop(scope_, &join, &update, &loop_);

  if (node->initialization())
    node->initialization()->accept(this);

  Label header, test;
  if (node->condition())
    emit_jump(&test);

  emit_bind(&header);
  node->body()->accept(this);

  emit_bind(&update);
  if (node->update())
    node->update()->accept(this);

  if (node->condition()) {
    HIRList output;
    emit_bind(&test);
    visitForTest(&output, node->condition(), &header, &join, &join);
    emit_hirlist(&output);
  } else {
    emit_jump(&header);
  }

  emit_bind(&join);
}

void
SemanticAnalysis::visitBreakStatement(BreakStatement *node)
{
  if (!loop_) {
    cc_.report(node->loc(), rmsg::break_outside_loop);
    return;
  }

  // Note, if we re-introduce LIFO allocs, we need to unwind here.
  emit_jump(loop_->break_());
}

void
SemanticAnalysis::visitContinueStatement(ContinueStatement *node)
{
  if (!loop_) {
    cc_.report(node->loc(), rmsg::continue_outside_loop);
    return;
  }

  // Note, if we re-introduce LIFO allocs, we need to unwind here.
  emit_jump(loop_->continue_());
}

void
SemanticAnalysis::visitReturnStatement(ReturnStatement *node)
{
#if 0
  if (!node->expression()) {
    if (!fun_->returnType()->isVoid()) {
      cc_.reportError(node->loc(), Message_UsedVoidReturn);
      return;
    }

    emit_return(node);
    return;
  }

  PoolScope hir_scope;
  begin_hir(hir_scope);

  HIR *hir = rvalue(node->expression());
  if (!hir)
    return;

  if ((hir = coerce(hir, fun_->returnType(), Coerce_Return)) == nullptr)
    return;

  emit_return(node, hir);
#endif
}

void
SemanticAnalysis::visitExpressionStatement(ExpressionStatement *stmt)
{
  PoolScope hir_scope;
  begin_hir(hir_scope);

  HIR *hir = rvalue(stmt->expression());
  if (!hir)
    return;

  emit_statement(stmt, hir);
}

void
SemanticAnalysis::visitCallExpr(CallExpr *node)
{
#if 0
  HIR *callee = rvalue(node->callee());
  if (!callee)
    return;

  if (!callee->type()->isFunction()) {
    cc_.reportError(node->loc(), Message_CalleeNotFunction);
    return;
  }

  FunctionType *fun = callee->type()->toFunction();
  
  if (!checkArgumentCount(fun, node->arguments()->length())) {
    cc_.reportError(node->loc(), Message_ArgumentCountMismatch);
    return;
  }

  if (fun->isForward()) {
    cc_.reportError(node->loc(), Message_ForwardNotImplemented, fun->name()->chars());
    return;
  }

  HIRList *args = new (pool_) HIRList;
  for (unsigned i = 0; i < node->arguments()->length(); i++) {
    Expression *expression = node->arguments()->at(i);

    Type *actual = nullptr;
    bool needs_reference = false;
    if (i >= fun->parameters()->length()) {
      assert(fun->isNative() && fun->isNativeVariadic());
      needs_reference = true;
    } else {
      actual = fun->parameterAt(i);
      needs_reference = actual->isReference() || actual->isArray();
    }

    HIR *hir = nullptr;
    if (!needs_reference) {
      if ((hir = rvalue(expression)) == nullptr)
        return;
      if ((hir = coerce(hir, actual, Coerce_Arg)) == nullptr)
        return;
    } else {
      SValue arg;
      if (!svalue(expression, &arg))
        return;

      // :TODO:
      assert(!arg.isRValue());

      if (arg.isLValue()) {
        const LValue &lval = arg.lvalue();
        if (actual && actual->isReference() && lval.isBaseIndex()) {
          // Disabled - this feature is dangerous.
          cc_.reportError(expression->loc(), Message_CannotComputeIndexRef);
          return;
        }
        // :TODO: need to type check.
        hir = new (pool_) HAddressOf(expression, lval);
      }
    }

    args->append(hir);
  }

  hir_ = new (pool_) HCall(node, fun->returnType(), callee, args);
#endif
}

bool
SemanticAnalysis::checkArgumentCount(FunctionType *type, unsigned actual)
{
#if 0
    if (type->parameters()->length() == actual)
        return true;

    if (actual > type->parameters()->length()) {
        if (!type->isNative() || !type->isNativeVariadic())
            return false;
        return true;
    }

    // :TODO:
    // if (!type->defaults())
    //     return false;

    // Missing arguments. See if there's a default argument for each missing
    // actual.
    // for (unsigned i = actual; i < type->parameters()->length(); i++) {
    //     if (!type->defaults()->at(i))
    //         return false;
    // }

#endif
    return true;
}

HIR *
SemanticAnalysis::coerce(HIR *hir, Type *to, CoercionKind kind)
{
  Type *from = hir->type();

  // If we're assigning a reference type, we just need to check that the
  // inner type matches.
  if (kind == Coerce_Assign && to->isReference())
    to = to->toReference()->contained();

  // if (from->isInt32() && to->isFloat())
  //   return new (pool_) HConvert(hir->node(), to, OP_CVT_I2F, hir);
  if (from->isInt32() && to->isBool())
    return hir; //new (pool_) HConvert(hir->node(), to, OP_CVT_I2B, hir);
  // 
  // if (to->isArray()) {
  //   if (!typeCheckArray(SourcePosition(), from, to, kind))
  //     return nullptr;;
  //   return hir;
  // }

  assert(from == to);
  
  return hir;
}

// Return only l-values and error if one cannot be found.
bool
SemanticAnalysis::lvalue(Expression *expr, LValue *outp)
{
  SValue out;

  if (!svalue(expr, &out))
    return false;

  if (!out.isLValue()) {
    cc_.report(expr->loc(), rmsg::expected_lvalue);
    return false;
  }

  *outp = out.lvalue();
  return true;
}

// Return only r-values and error if one cannot be found.
HIR *
SemanticAnalysis::rvalue(Expression *expr)
{
  // Demand only RValues.
  SaveAndSet<ValueContext> context(&value_context_, kRValue);

  // It's not strictly necessary to do this but it makes state cleaner.
  SaveAndSet<LValue *> lvalue(&outp_, nullptr);

  assert(!outp_ && !hir_);
  expr->accept(this);

  return ReturnAndVoid(hir_);
}

// If an l-value is found, return it. Otherwise, return an r-value.
bool
SemanticAnalysis::svalue(Expression *expr, SValue *outp)
{
  // Demand only RValues.
  SaveAndSet<ValueContext> context(&value_context_, kLValue);

  LValue lval;

  // Between setting the "outparams" and returning, nothing should call us.
  assert(!outp_ && !hir_);
  outp_ = &lval;
  expr->accept(this);
  outp_ = nullptr;

  // We should not have received both an r-value and an l-value.
  assert(!hir_ || lval.kind() == LValue::Error);
  if (!hir_ && lval.kind() == LValue::Error)
    return false;

  if (hir_)
    *outp = SValue(ReturnAndVoid(hir_));
  else
    *outp = SValue(lval);
  return true;
}
