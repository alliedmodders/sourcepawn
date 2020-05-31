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
#ifndef _include_spcomp_type_resolver_h_
#define _include_spcomp_type_resolver_h_

#include "parser/ast.h"
#include "scopes.h"
#include "symbols.h"
#include "constant-evaluator.h"
#include "types.h"
#include <amtl/am-deque.h>

namespace sp {

using namespace ke;
using namespace ast;

class TranslationUnit;
class TypeSpecHelper;
class VariableSymbol;

enum class RankStatus
{
  // No literals were available for this rank.
  Unvisited,

  // Sizes did not match across literals.
  Indeterminate,

  // A size was already given by a constant expression, but it will be computed
  // elsewhere.
  Determinate,

  // A size was computed via a fixed literal.
  Computed
};

struct Rank
{
  Rank()
   : status(RankStatus::Unvisited),
     size(0)
  {}
  explicit Rank(RankStatus status, int size)
   : status(status),
     size(size)
  {}
  RankStatus status;
  int size;
};

class TypeResolver
 : public StrictAstVisitor,
   public ConstantResolver
{
 public:
  TypeResolver(CompileContext& cc);

  bool analyze();

  Type* resolveTypeOfVar(VariableSymbol* sym) override;
  bool resolveVarAsConstant(VariableSymbol* sym, BoxedValue* out) override;
  BoxedValue resolveValueOfConstant(ConstantSymbol* sym) override;
  void visitVarDecl(VarDecl* node) override;
  void visitEnumConstant(EnumConstant* node) override;
  void visitEnumStatement(EnumStatement* node) override;
  void visitMethodmapDecl(MethodmapDecl* methodmap) override;
  void visitFieldDecl(FieldDecl* decl) override;
  void visitPropertyDecl(PropertyDecl* decl) override;
  void visitMethodDecl(MethodDecl* decl) override;
  void visitFunctionStatement(FunctionStatement* node) override;
  void visitTypedefDecl(TypedefDecl* node) override;
  void visitViewAsExpression(ViewAsExpression* expr) override;
  void visitCallNewExpr(CallNewExpr* expr) override;
  void visitNewArrayExpr(NewArrayExpr* expr) override;
  void visitTypesetDecl(TypesetDecl* decl) override;

  void addPending(AstNode* node) {
    work_queue_.push_back(node);
  }

  Type* applyConstQualifier(TypeSpecifier* spec, Type* type);
  Type* applyByRef(TypeSpecifier* spec, Type* type, TypeSpecHelper* helper);
  Type* applyVariadic(TypeSpecifier* spec, Type* type, TypeSpecHelper* helper);
  bool checkArrayInnerType(TypeSpecifier* spec, Type* type);
  bool verifyTypeset(TypesetDecl* decl);

  bool assignTypeToSymbol(VariableSymbol* sym, Type* type);
  void assignTypeToFunction(FunctionNode* node);
  void assignTypeToTypedef(TypedefDecl* decl, TypedefType* def, Type* actual);

 private:
  EnumType* resolveMethodmapParentType(NameProxy* proxy);
  void visitFunction(FunctionNode* node);

 private:
  FunctionType* maybeResolveFunction(FunctionSignature* sig);

  bool resolveConstantArraySize(ConstantEvaluator::Mode, Expression* expr, int* outp);
  void computeFixedArraySizes(TypeSpecifier* spec,
                              Type* base,
                              std::vector<Rank>& ranks,
                              size_t rank_index,
                              ArrayLiteral* expr);
  std::vector<Rank> fixedArrayLiteralDimensions(TypeSpecifier* spec, Type* base, Expression* expr);

  Type* resolveTypeIfNeeded(TypeExpr& te) {
    if (te.resolved())
      return te.resolved();
    return resolveType(te);
  }

  void resolveTypesInSignature(FunctionSignature* sig);
  void resolveConstant(ConstantSymbol* sym);
  Type* resolveType(TypeExpr& te, TypeSpecHelper* helper = nullptr);
  Type* resolveBaseType(TypeSpecifier* spec);
  Type* resolveNameToType(NameProxy* proxy);
  Type* resolveArrayComponentTypes(TypeSpecifier* spec,
                                   Type* type,
                                   TypeSpecHelper* helper);
  bool resolveEnumConstantValue(EnumConstant* cs, int* outp);

 private:
  PoolAllocator& pool_;
  CompileContext& cc_;

  std::deque<AstNode*> work_queue_;

  std::vector<EnumConstant*> enum_constant_stack_;
};

// Certain keywords, like "const", may need contextual information to finish
// resolving. Callsites may provide information and callbacks via
// TypeSpecHelper.
class TypeSpecHelper
{
 public:
  virtual Expression* initializer() const {
    return nullptr;
  }
  virtual VarDecl* decl() const {
    return nullptr;
  }
};

class VarDeclSpecHelper : public TypeSpecHelper
{
 public:
  VarDeclSpecHelper(VarDecl* decl, Expression* initializer)
   : decl_(decl),
     initializer_(initializer)
  {}

  VarDecl* decl() const override {
    return decl_;
  }
  Expression* initializer() const override {
    return initializer_;
  }

 private:
  VarDecl* decl_;
  Expression* initializer_;
};

} // namespace sp

#endif // _include_spcomp_type_resolver_h_
