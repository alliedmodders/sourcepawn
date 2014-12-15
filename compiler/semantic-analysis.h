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
#ifndef _include_semantic_analysis_h_
#define _include_semantic_analysis_h_

#include "ast.h"
#include <am-utility.h>

namespace sp {

class CompileContext;
class PoolAllocator;

class SemanticAnalysis : public StrictAstVisitor
{
 public:
  SemanticAnalysis(CompileContext &cc, TranslationUnit *unit);

  bool analyze();

 public:
  void visitFunctionStatement(FunctionStatement *node) override;
  void visitBlockStatement(BlockStatement *node) override;
  void visitExpressionStatement(ExpressionStatement *node) override;
  void visitCallExpr(CallExpr *node) override;

private:
  void analyzeShadowedFunctions(FunctionSymbol *sym);
  void checkForwardedFunction(FunctionStatement *forward, FunctionStatement *impl); 
  bool matchForwardSignatures(FunctionSignature *fwdSig, FunctionSignature *implSig);
  bool matchForwardReturnTypes(Type *fwdRetType, Type *implRetType);

  void checkCall(FunctionSignature *sig, ExpressionList *args);

  // Visit for any kind of value.
  Type *visitForValue(Expression *expr);

  // Turn l-values into r-values.
  Type *visitForRValue(Expression *expr);

private:
  // For type-checking diagnostics, we track reasons for why type checking
  // failed.
  enum class CR {
    // No conversion was required.
    ok,

    // Some kind of conversion was required.
    converted,

    // Almost-success conditions.
    discards_const_qualifiers,
    loses_precision,
    ambiguous,
    ref_is_readonly,
    ref_expr_not_lvalue,

    // Failure conditions.
    ref_inner_type_mismatch,
    cant_coerce_to_metafunction,
    cant_coerce_to_function,
    cant_coerce_to_struct,
    cant_coerce_to_array,
    cant_coerce_to_int,
    cant_coerce_to_enum,
    cant_coerce_to_typeset,
    cant_coerce_to_float,
    cant_coerce_to_bool,
    cant_coerce_to_null,
    cant_coerce_to_fixedarray,
    function_type_mismatch,
    typeset_const_mismatch,
    unchecked_type_mismatch,
    enum_type_mismatch,
    unexpected_type,

    sentinel
  };

  // This class is used for handlign implicit coercions.
  struct Coercion {
    enum Context {
      arg,
      assign,
      shallow
    };

    // Setup.
    Context context;
    ReportingContext rr;

    // Inputs.
    Expression *expr;
    Type *to;

    // Outputs.
    ExprOp *op;
    Ref<TMessage> message;

    Coercion(CompileContext &cc, Expression *expr, Context context, Type *to)
     : context(context),
       rr(cc, expr->loc()),
       expr(expr),
       to(to),
       op(expr->op())
    {
    }

    Type *from() const {
      if (op)
        return op->type();
      return expr->type();
    }
    VK vk() const {
      if (op)
        return op->vk();
      return expr->vk();
    }
  };

  // Perform a coercion.
  CR do_coerce(Coercion &cr);

  // Attempt a coercion, but do not commit the result.
  CR try_coerce(Coercion &cr, Type *from, Type *to);

  // Coercion helpers.
  CR coerceInner(Coercion &cr, Type *from, Type *to);
  CR coerceToRef(Coercion &cr, Type *from, Type *to);
  CR coerceToTypeset(Coercion &cr, Type *from, Type *to);
  CR coerceToInt(Coercion &cr, Type *from, Type *to);
  CR coerceToBool(Coercion &cr, Type *from, Type *to);
  CR coerceToFloat(Coercion &cr, Type *from, Type *to);
  CR coerceToFixedArray(Coercion &cr, Type *from, Type *to);
  CR coerceToArray(Coercion &cr, Type *from, Type *to);
  CR maybeCoerceToConst(Coercion &cr, CR status, Type *to);

  // Maybe add a diagnostic note for a failed coercion.
  MessageBuilder diag(const SourceLocation &loc, CR status);

 private:
  struct FuncState : StackLinked<FuncState>
  {
    FunctionNode *fun;

    FuncState(FuncState **prev, FunctionNode *node)
     : StackLinked<FuncState>(prev),
       fun(node)
    {}
  };

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  TranslationUnit *tu_;

  FuncState *funcstate_;
};

}

#endif // _include_semantic_analysis_h_
