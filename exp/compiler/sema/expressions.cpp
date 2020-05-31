// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC and David Anderson
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
#include <list>

#include "coercion.h"
#include "compile-context.h"
#include "parser/ast.h"
#include "scopes.h"
#include "semantic-analysis.h"
#include "symbols.h"

namespace sp {

using namespace ke;
using namespace ast;

sema::Expr*
SemanticAnalysis::visitExpression(Expression* node)
{
  switch (node->kind()) {
    case AstKind::kIntegerLiteral:
      return visitIntegerLiteral(node->toIntegerLiteral());
    case AstKind::kBinaryExpression:
      return visitBinaryExpression(node->toBinaryExpression());
    case AstKind::kCallExpression:
      return visitCallExpression(node->toCallExpression());
    case AstKind::kNameProxy:
      return visitNameProxy(node->toNameProxy());
    case AstKind::kUnaryExpression:
      return visitUnaryExpression(node->toUnaryExpression());
    case AstKind::kStringLiteral:
      return visitStringLiteral(node->toStringLiteral());
    case AstKind::kIncDecExpression:
      return visitIncDec(node->toIncDecExpression());
    case AstKind::kAssignment:
      return visitAssignment(node->toAssignment());
    case AstKind::kIndexExpression:
      return visitIndex(node->toIndexExpression());
    case AstKind::kCharLiteral:
      return visitCharLiteral(node->toCharLiteral());
    case AstKind::kNewArrayExpr:
      return visitNewArray(node->toNewArrayExpr());
    case AstKind::kTernaryExpression:
      return visitTernary(node->toTernaryExpression());
    case AstKind::kSizeofExpression:
      return visitSizeof(node->toSizeofExpression());
    case AstKind::kViewAsExpression:
      return visitViewAs(node->toViewAsExpression());
    default:
      cc_.report(node->loc(), rmsg::unimpl_kind) <<
        "sema-visit-expr" << node->kindName();
      return nullptr;
  }
  return nullptr;
}

sema::CallExpr*
SemanticAnalysis::visitCallExpression(CallExpression* node)
{
  // Call expressions are complicated because we only support very specific
  // patterns. We sniff them out here.
  sema::Expr* callee = nullptr;
  if (NameProxy* proxy = node->callee()->asNameProxy()) {
    if (FunctionSymbol* sym = proxy->sym()->asFunction()) {
      assert(sym->scope()->kind() == Scope::Global);
      callee = new (pool_) sema::NamedFunctionExpr(proxy, sym->impl()->signature_type(), sym);
    }
  }

  if (!callee || !callee->type()->isFunction()) {
    cc_.report(node->loc(), rmsg::callee_is_not_function);
    return nullptr;
  }

  FunctionType* fun_type = callee->type()->asFunction();
  ast::FunctionSignature* sig = fun_type->signature();
  ast::ParameterList* params = sig->parameters();
  ast::ExpressionList* ast_args = node->arguments();

  // If this function is variadic, extract the variadic parameter.
  Type* vararg = nullptr;
  size_t normal_argc = params->size();
  if (params->size() > 0) {
    VariableSymbol* sym = params->back()->sym();
    if (sym->type()->isVariadic()) {
      vararg = sym->type();
      normal_argc = params->size() - 1;
    }
  }

  if ((vararg && ast_args->size() < params->size() - 1) ||
      (!vararg && ast_args->size() != params->size()))
  {
    cc_.report(node->loc(), rmsg::incorrect_argcount) <<
      params->size() << ast_args->size();
  }

  sema::ExprList* args = new (pool_) sema::ExprList();
  for (size_t i = 0; i < ast_args->size(); i++) {
    ast::Expression* ast_arg = ast_args->at(i);

    Type* arg_type = nullptr;
    if (i < normal_argc) {
      VarDecl* param = params->at(i);
      VariableSymbol* sym = param->sym();
      arg_type = sym->type();
    } else {
      if (!vararg)
        break;
      arg_type = vararg;
    }

    sema::Expr* result = coerce_arg(ast_arg, arg_type);
    if (!result)
      return nullptr;
    args->push_back(result);
  }

  return new (pool_) sema::CallExpr(node, fun_type->returnType(), callee, args);
}

sema::ConstValueExpr*
SemanticAnalysis::visitIntegerLiteral(IntegerLiteral* node)
{
  // :TODO: test overflow
  int32_t value;
  if (!IntValue::SafeCast(node->value(), &value)) {
    cc_.report(node->loc(), rmsg::int_literal_out_of_range);
    return nullptr;
  }

  BoxedValue b(IntValue::FromValue(value));

  Type* i32type = types_->getPrimitive(PrimitiveType::Int32);
  return new (pool_) sema::ConstValueExpr(node, i32type, b);
}

sema::Expr*
SemanticAnalysis::visitNameProxy(ast::NameProxy* node)
{
  Symbol* base_sym = node->sym();
  VariableSymbol* sym = base_sym->asVariable();
  if (!sym) {
    cc_.report(node->loc(), rmsg::unimpl_kind) <<
      "name-proxy-symbol" << node->kindName();
    return nullptr;
  }

  return new (pool_) sema::VarExpr(node, sym->type(), sym);
}

sema::BinaryExpr*
SemanticAnalysis::visitBinaryExpression(BinaryExpression* node)
{
  sema::Expr* left = visitExpression(node->left());
  if (!left)
    return nullptr;

  sema::Expr* right = visitExpression(node->right());
  if (!right)
    return nullptr;

  // Logical operators need booleans on both sides.
  EvalContext ec_left, ec_right;
  if (node->token() == TOK_OR || node->token() == TOK_AND) {
    ec_left = TestEvalContext(cc_, left);
    ec_right = TestEvalContext(cc_, right);
  } else {
    Type* int32Type = types_->getPrimitive(PrimitiveType::Int32);
    ec_left = EvalContext(CoercionKind::Expr, left, int32Type);
    ec_right = EvalContext(CoercionKind::Expr, right, int32Type);
  }

  if (!coerce(ec_left) || !coerce(ec_right))
    return nullptr;
  left = ec_left.result;
  right = ec_right.result;

  assert(left->type() == right->type());

  Type* type = nullptr;
  switch (node->token()) {
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
      type = left->type();
      break;
    case TOK_EQUALS:
    case TOK_NOTEQUALS:
    case TOK_GT:
    case TOK_GE:
    case TOK_LT:
    case TOK_LE:
    case TOK_OR:
    case TOK_AND:
      type = types_->getBool();
      break;
    default:
      cc_.report(node->loc(), rmsg::unimpl_kind) <<
        "sema-bin-token" << TokenNames[node->token()];
      return nullptr;
  }

  return new (pool_) sema::BinaryExpr(node, type, node->token(), left, right);
}

sema::Expr*
SemanticAnalysis::visitUnaryExpression(ast::UnaryExpression* node)
{
  EvalContext ec;
  if (node->token() == TOK_NOT) {
    ec = TestEvalContext(cc_, node->expression());
  } else {
    Type* type = types_->getPrimitive(PrimitiveType::Int32);
    ec = EvalContext(CoercionKind::Expr, node->expression(), type);
  }

  if (!coerce(ec))
    return nullptr;

  return new (pool_) sema::UnaryExpr(node, ec.to, node->token(), ec.result);
}

sema::Expr*
SemanticAnalysis::visitIndex(ast::IndexExpression* node)
{
  sema::Expr* base = visitExpression(node->left());
  if (!base)
    return nullptr;

  if (!base->type()->isArray()) {
    cc_.report(base->src()->loc(), rmsg::cannot_index_type) <<
      base->type();
    return nullptr;
  }

  // Convert the base to an r-value.
  LValueToRValueContext base_ec(base);
  if (!coerce(base_ec))
    return nullptr;
  base = base_ec.result;

  // Make sure the index is an integer.
  Type* int32Type = types_->getPrimitive(PrimitiveType::Int32);
  EvalContext index_ec(CoercionKind::Index, node->right(), int32Type);
  if (!coerce(index_ec))
    return nullptr;

  sema::Expr* index = index_ec.result;
  ArrayType* array = base->type()->toArray();

  int32_t value;
  if (index->getConstantInt32(&value)) {
    if (value < 0) {
      cc_.report(index->src()->loc(), rmsg::index_must_be_positive);
      return nullptr;
    }
    if (array->hasFixedLength() && value >= array->fixedLength()) {
      cc_.report(index->src()->loc(), rmsg::index_out_of_bounds);
      return nullptr;
    }
  }

  // We let the backend decide the internal structure of a multi-dimensional
  // array. Semantically, however, if an index operation yields an unsized
  // array, then the l-value is a pointer to the array reference. If the
  // operation yields a fixed-size array, then the l-value is the array
  // reference itself.
  //
  // If the backend decides to treat both cases identically (for example,
  // as smx-v1 does with indirection vectors), then it is responsible for
  // emitting a load.
  //
  // Closer to release, when we can better future-proof array semantics,
  // we may find that indirection vectors are a necessity and thus always
  // insert a Load here.
  //
  // :TODO: tests.
  return new (pool_) sema::IndexExpr(node, array->contained(), base, index);
}

sema::Expr*
SemanticAnalysis::visitStringLiteral(ast::StringLiteral* node)
{
  Type* charType = types_->getPrimitive(PrimitiveType::Char);
  Type* constCharType = types_->newQualified(charType, Qualifiers::Const);
  Type* strLitType = types_->newArray(constCharType, node->arrayLength());

  return new (pool_) sema::StringExpr(node, strLitType, node->literal());
}

sema::Expr*
SemanticAnalysis::visitCharLiteral(ast::CharLiteral* node)
{
  BoxedValue b(IntValue::FromSigned(node->value(), 32));
  Type* charType = types_->getPrimitive(PrimitiveType::Char);
  return new (pool_) sema::ConstValueExpr(node, charType, b);
}

// :TODO: write tests for weird operators on weird types, like ++function or function - function.
static bool
IsValidIncDecType(Type* type)
{
  switch (type->canonicalKind()) {
    case Type::Kind::Primitive:
      return type->primitive() == PrimitiveType::Int32 ||
             type->primitive() == PrimitiveType::Char;
    default:
      return false;
  }
}

sema::Expr*
SemanticAnalysis::visitIncDec(ast::IncDecExpression* node)
{
  sema::LValueExpr* expr = visitLValue(node->expression());
  if (!expr)
    return nullptr;

  Type* type = expr->storedType();
  if (type->isConst()) {
    cc_.report(node->loc(), rmsg::lvalue_is_const);
    return nullptr;
  }

  // We define this operator for untagged integral types.
  if (!IsValidIncDecType(type)) {
    cc_.report(node->loc(), rmsg::operator_not_defined) <<
      TokenNames[node->token()] << type;
    return nullptr;
  }

  return new (pool_) sema::IncDecExpr(node, type, node->token(), expr, node->postfix());
}

static inline TokenKind
AssignOpToNormalOp(TokenKind op)
{
  switch (op) {
    case TOK_ASSIGN_ADD:
      return TOK_PLUS;
    case TOK_ASSIGN_SUB:
      return TOK_MINUS;
    case TOK_ASSIGN_MUL:
      return TOK_STAR;
    case TOK_ASSIGN_DIV:
      return TOK_SLASH;
    case TOK_ASSIGN_MOD:
      return TOK_PERCENT;
    case TOK_ASSIGN_BITAND:
      return TOK_BITAND;
    case TOK_ASSIGN_BITOR:
      return TOK_BITOR;
    case TOK_ASSIGN_BITXOR:
      return TOK_BITXOR;
    case TOK_ASSIGN_SHR:
      return TOK_SHR;
    case TOK_ASSIGN_USHR:
      return TOK_USHR;
    case TOK_ASSIGN_SHL:
      return TOK_SHL;
    default:
      assert(false);
      return TOK_NONE;
  }
}

sema::Expr*
SemanticAnalysis::visitAssignment(ast::Assignment* node)
{
  sema::LValueExpr* lhs = visitLValue(node->lvalue());
  if (!lhs)
    return nullptr;

  Type* type = lhs->storedType();

  if (type->isConst()) {
    cc_.report(node->loc(), rmsg::lvalue_is_const);
    return nullptr;
  }

  // Prevent deep copies if the interior is const.
  if (ArrayType* array = type->asArray()) {
    if (array->hasFixedLength() && array->contained()->isConst()) {
      cc_.report(node->loc(), rmsg::lvalue_is_const);
      return nullptr;
    }

    // :TODO:
    assert(false);
  }

  // :TODO: test
  assert(!type->isStruct());

  sema::Expr* rhs = nullptr;
  if (node->token() != TOK_ASSIGN) {
    // :TODO: test with weird types like bool
    EvalContext left_ec(CoercionKind::Expr, lhs, type);
    if (!coerce(left_ec))
      return nullptr;

    EvalContext right_ec(CoercionKind::Expr, node->expression(), type);
    if (!coerce(right_ec))
      return nullptr;

    rhs = new (pool_) sema::BinaryExpr(
      node,
      type,
      AssignOpToNormalOp(node->token()),
      left_ec.result,
      right_ec.result);
  } else {
    EvalContext right_ec(CoercionKind::Assignment, node->expression(), type);
    if (!coerce(right_ec))
      return nullptr;
    rhs = right_ec.result;
  }

  return new (pool_) sema::StoreExpr(node, type, lhs, rhs);
}

sema::LValueExpr*
SemanticAnalysis::visitLValue(ast::Expression* node)
{
  sema::Expr* expr = visitExpression(node);
  if (!expr)
    return nullptr;

  sema::LValueExpr* lv = expr->asLValueExpr();
  if (!lv) {
    cc_.report(node->loc(), rmsg::illegal_lvalue);
    return nullptr;
  }

  return lv;
}

sema::Expr*
SemanticAnalysis::initializer(ast::Expression* node, Type* type)
{
  if (StructInitializer* init = node->asStructInitializer())
    return struct_initializer(init, type);
  if (ArrayLiteral* init = node->asArrayLiteral())
    return array_initializer(init, type);

  sema::Expr* expr = visitExpression(node);
  if (!expr)
    return nullptr;

  if (type->isArray() && !type->toArray()->hasFixedLength()) {
    // Currently, assigning array pointers is invalid. We make an exception
    // for two cases that SP1 can support. When we support fully dynamic
    // arrays, none of this will matter.
    if (type->isCharArray() && expr->asStringExpr())
      return expr;
    if (expr->asNewArrayExpr()) {
      if (!isValidNewArrayInitializer(expr->type(), type)) {
        cc_.report(node->loc(), rmsg::cannot_coerce) <<
          expr->type() << type;
        return nullptr;
      }
      return expr;
    }

    // No initializer will work.
    rmsg::Id msg = type->isCharArray()
                   ? rmsg::dynamic_array_needs_new_or_str
                   : rmsg::dynamic_array_needs_new;
    cc_.report(node->loc(), msg);
    return nullptr;
  }

  // This is a very special exception in the SP1 compiler. When we support
  // fully dynamic arrays, this won't matter.
  if (expr->asStringExpr() &&
      type->isCharArray() &&
      !type->toArray()->hasFixedLength())
  {
    return expr;
  }

  // :TODO: check overflow integers
  EvalContext ec(CoercionKind::Assignment, expr, type);
  if (!coerce(ec))
    return nullptr;
  return ec.result;
}

sema::Expr*
SemanticAnalysis::array_initializer(ast::ArrayLiteral* expr, Type* type)
{
  if (!type->isArray()) {
    cc_.report(expr->loc(), rmsg::array_literal_with_non_array) <<
      type;
    return nullptr;
  }

  ArrayType* array = type->toArray();
  if (!array->hasFixedLength()) {
    cc_.report(expr->loc(), rmsg::array_literal_with_dynamic_array) <<
      type;
    return nullptr;
  }

  if (expr->arrayLength() > array->fixedLength()) {
    cc_.report(expr->loc(), rmsg::array_literal_too_many_exprs) <<
      expr->arrayLength() << array->fixedLength();
    return nullptr;
  }

  // We don't care about qualifiers for array initialization.
  Type* contained = array->contained()->unqualified();

  bool all_const = true;
  FixedPoolList<sema::Expr*>* list = new (pool_) FixedPoolList<sema::Expr*>(expr->arrayLength());

  for (size_t i = 0; i < expr->expressions()->size(); i++) {
    ast::Expression* src = expr->expressions()->at(i);
    sema::Expr* val;
    if (src->isArrayLiteral()) {
      val = array_initializer(src->toArrayLiteral(), contained);
    } else {
      EvalContext ec(CoercionKind::Assignment, src, contained);
      if (!coerce(ec))
        return nullptr;
      val = ec.result;
    }
    if (!val)
      return nullptr;
    if (!val->isConstant()) {
      // Currently, this is an error, but we could change this in the future.
      cc_.report(src->loc(), rmsg::array_literal_must_be_const);
      return nullptr;
    }
    list->at(i) = val;
  }

  sema::ArrayInitExpr* init = new (pool_) sema::ArrayInitExpr(expr, type, list);
  if (all_const)
    init->set_all_const();
  if (expr->repeatLastElement())
    init->set_repeat_last_element();
  return init;
}

sema::Expr*
SemanticAnalysis::struct_initializer(ast::StructInitializer* expr, Type* type)
{
  if (!type->isStruct()) {
    cc_.report(expr->loc(), rmsg::struct_init_needs_struct_type);
    return nullptr;
  }

  std::list<ast::NameAndValue*> entries;
  for (ast::NameAndValue* item : *expr->pairs())
    entries.push_back(item);

  StructType* st = type->asStruct();
  ast::RecordDecl* decl = st->decl();
  ast::LayoutDecls* body = decl->body();

  PoolList<sema::Expr*>* out = new (pool_) PoolList<sema::Expr*>();

  size_t nfields = 0;
  for (ast::LayoutDecl* decl : *body) {
    FieldDecl* field = decl->asFieldDecl();
    if (!field)
      continue;

    nfields++;

    FieldSymbol* sym = field->sym();
    NameAndValue* assignment = nullptr;

    /* Find a matching assignment. */
    auto iter = entries.begin();
    while (iter != entries.end()) {
      NameAndValue* nv = (*iter);
      if (nv->name() == sym->name()) {
        if (assignment) {
          cc_.report(nv->expr()->loc(), rmsg::struct_init_appears_twice) <<
            nv->name();
        }

        assignment = nv;
        iter = entries.erase(iter);
      } else {
        iter++;
      }
    }

    // The backend must generate a default initializer.
    if (!assignment) {
      out->push_back(nullptr);
      continue;
    }

    // We only support two types here: int, and string.
    sema::Expr* value = visitExpression(assignment->expr());
    if (!value)
      continue;

    if (sym->type()->isCharArray()) {
      sema::StringExpr* str = value->asStringExpr();
      if (!str) {
        cc_.report(value->src()->loc(), rmsg::struct_init_needs_string_lit) <<
          sym->name();
        continue;
      }
    } else if (sym->type()->isPrimitive(PrimitiveType::Int32)) {
      sema::ConstValueExpr* cv = value->asConstValueExpr();
      if (!cv ||
          !cv->value().isInteger() ||
          !cv->value().toInteger().valueFitsInInt32())
      {
        cc_.report(value->src()->loc(), rmsg::struct_init_needs_string_lit) <<
          sym->name();
        continue;
      }
    } else {
      cc_.report(decl->loc(), rmsg::struct_unsupported_type) <<
        sym->name() << sym->type();
      continue;
    }

    out->push_back(value);
  }

  for (NameAndValue* nv : entries) {
    cc_.report(nv->loc(), rmsg::struct_field_not_found) <<
      st->name() << nv->name();
  }

  if (out->size() != nfields)
    return nullptr;

  return new (pool_) sema::StructInitExpr(expr, st, out);
}

bool
SemanticAnalysis::isValidNewArrayInitializer(Type* from, Type* to)
{
  ArrayType* from_array = from->toArray();
  ArrayType* to_array = to->toArray();
  while (true) {
    from = from_array->contained();
    to = to_array->contained();

    from_array = from->asArray();
    to_array = to->asArray();
    if (!from_array && !to_array)
      break;

    // If one is null but not the other, the assignment is invalid.
    if (!from_array || !to_array)
      return false;
  }

  return CompareNonArrayTypesExactly(from, to);
}

sema::Expr*
SemanticAnalysis::visitNewArray(ast::NewArrayExpr* node)
{
  Type* base = node->te().resolved();
  if (base->isArray()) {
    cc_.report(node->loc(), rmsg::new_array_illegal_array_base);
    return nullptr;
  }
  if (base->isConst()) {
    cc_.report(node->loc(), rmsg::new_array_illegal_const_base);
    return nullptr;
  }

  Type* int32Type = types_->getPrimitive(PrimitiveType::Int32);

  sema::FixedExprList* exprs = new (pool_) sema::FixedExprList(node->dims()->size());
  Type* type = base;
  for (size_t i = 0; i < node->dims()->size(); i++) {
    ast::Expression* ast_expr = node->dims()->at(i);
    if (!ast_expr) {
      cc_.report(node->loc(), rmsg::new_array_missing_dimension) << i;
      return nullptr;
    }

    EvalContext ec(CoercionKind::Index, ast_expr, int32Type);
    if (!coerce(ec))
      return nullptr;
    exprs->at(i) = ec.result;

    type = types_->newArray(type, ArrayType::kUnsized);
  }

  // :TODO: test old syntax for genarray.
  return new (pool_) sema::NewArrayExpr(node, type, exprs);
}

sema::Expr*
SemanticAnalysis::visitTernary(ast::TernaryExpression* node)
{
  TestEvalContext test_ec(cc_, node->condition());
  if (!coerce(test_ec))
    return nullptr;

  TernaryContext tc(node->left(), node->right());
  if (!coerce_ternary(tc))
    return nullptr;

  return new (pool_) sema::TernaryExpr(
    node,
    tc.type,
    test_ec.result,
    tc.left,
    tc.right);
}

sema::Expr*
SemanticAnalysis::visitSizeof(ast::SizeofExpression* node)
{
  VariableSymbol* sym = node->proxy()->sym()->asVariable();
  if (!sym) {
    cc_.report(node->loc(), rmsg::sizeof_needs_variable);
    return nullptr;
  }

  Type* type = UnwrapReference(sym->type());
  for (size_t i = 1; i <= node->level(); i++) {
    if (!type->isArray()) {
      if (i == 1)
        cc_.report(node->loc(), rmsg::sizeof_needs_array);
      else
        cc_.report(node->loc(), rmsg::sizeof_invalid_rank);
      return nullptr;
    }
    type = type->toArray()->contained();
  }

  int32_t result = 1;
  switch (type->canonicalKind()) {
    case Type::Kind::Array:
    {
      ArrayType* array = type->asArray();
      if (!array) {
        cc_.report(node->loc(), rmsg::sizeof_indeterminate);
        return nullptr;
      }
      result = array->fixedLength();
      break;
    }
    case Type::Kind::Primitive:
    case Type::Kind::Typeset:
    case Type::Kind::Function:
    case Type::Kind::MetaFunction:
    case Type::Kind::Enum:
    case Type::Kind::Unchecked:
    case Type::Kind::NullType:
      result = 1;
      break;
    default:
      cc_.report(node->loc(), rmsg::sizeof_unsupported_type) <<
        type;
      return nullptr;
  }

  IntValue iv = IntValue::FromValue<int32_t>(result);
  return new (pool_) sema::ConstValueExpr(
    node,
    types_->getPrimitive(PrimitiveType::Int32),
    BoxedValue(iv));
}

sema::Expr*
SemanticAnalysis::visitViewAs(ast::ViewAsExpression* node)
{
  sema::Expr* expr = visitExpression(node->expr());
  if (!expr)
    return nullptr;

  // This will not let us cast addresses, we'll have to fix that eventually.
  EvalContext ec(CoercionKind::RValue, expr, expr->type());
  if (!coerce(ec))
    return nullptr;

  Type* to = node->te().resolved();
  Type* from = ec.result->type();
  if (!IsLegacyCellType(to) || !IsLegacyCellType(from)) {
    cc_.report(node->loc(), rmsg::cannot_view_as_to) <<
      from << to;
    return nullptr;
  }

  return new (pool_) sema::ImplicitCastExpr(
    node,
    to,
    sema::CastOp::None,
    ec.result);
}

} // namespace sp
