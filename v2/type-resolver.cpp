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
#include "compile-context.h"
#include "compile-phases.h"
#include "ast.h"
#include "scopes.h"
#include "symbols.h"
#include "constant-evaluator.h"
#include "types.h"

using namespace ke;

class TypeResolver : public AstVisitor
{
  class EnterScope : public SaveAndSet<Scope *>
  {
   public:
    EnterScope(Scope **loc, Scope *scope)
     : SaveAndSet<Scope *>(loc, scope ? scope : *loc)
    { }
  };

 public:
  TypeResolver(CompileContext &cc, TranslationUnit *unit)
   : pool_(cc.pool()),
     cc_(cc),
     unit_(unit),
     fun_(nullptr),
     scope_(nullptr)
  {
  }

  bool analyze() {
    EnterScope scope(&scope_, unit_->globalScope());

    for (size_t i = 0; i < unit_->tree()->statements()->length(); i++) {
      Statement *stmt = unit_->tree()->statements()->at(i);
      stmt->accept(this);
    }

    return true;
  }

  void visitVariableDeclaration(VariableDeclaration *node) {
    Vector<int> literal_dims;
    if (Expression *init = node->initialization()) {
      // Compute the dimensions of initializers in case the declaration type
      // requires inference.
      if (ArrayLiteral *lit = init->asArrayLiteral()) {
        literal_dims = fixedArrayLiteralDimensions(node->spec(), lit);
      } else if (StringLiteral *lit = init->asStringLiteral()) {
        literal_dims.append(lit->arrayLength());
      }
    }

    resolveType(node->spec(), &literal_dims);
  }

  void visitEnumStatement(EnumStatement *node) {
    int value = 0;
    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumStatement::Entry &entry = node->entries()->at(i);
      if (entry.expr) {
        BoxedPrimitive out;
        ConstantEvaluator ceval(cc_, scope_, ConstantEvaluator::Required);
        switch (ceval.Evaluate(entry.expr, &out)) {
          case ConstantEvaluator::Ok:
            if (!out.isInt()) {
              cc_.reportError(entry.proxy->loc(), Message_EnumConstantMustBeInt);
              continue;
            }
            break;
          case ConstantEvaluator::NotConstant:
            cc_.reportError(entry.proxy->loc(), Message_EnumValueMustBeConstant);
            break;
          default:
            continue;
        }
        value = out.toInt();
      }

      entry.sym->setValue(BoxedPrimitive::Int(value));
      value++;
    }
  }

  void visitLayoutStatement(LayoutStatement *layout) {
    for (size_t i = 0; i < layout->body()->length(); i++) {
      LayoutEntry *entry = layout->body()->at(i);
      switch (entry->type()) {
        case LayoutEntry::Accessor:
          if (entry->getter().isFunction())
            visitFunction(entry->getter().fun());
          if (entry->setter().isFunction())
            visitFunction(entry->setter().fun());
          break;

        case LayoutEntry::Method:
          if (entry->method().isFunction())
            visitFunction(entry->method().fun());
          break;
      }
    }
  }

  void visitFunction(FunctionNode *node) {
    resolveTypesInSignature(node->signature());

    SaveAndSet<FunctionNode *> save(&fun_, node);
    EnterScope funScope(&scope_, node->funScope());
    EnterScope varScope(&scope_, node->varScope());

    if (node->body())
      node->body()->accept(this);
  }

  void visitFunctionStatement(FunctionStatement *node) {
    visitFunction(node);
  }

  void visitTypedefStatement(TypedefStatement *node) {
    Type *type = resolveType(node->spec());

    TypedefType *alias = node->sym()->type()->toTypedef();
    alias->resolve(type);

    // We already resolved this type earlier. We have to make sure now that it
    // does not resolve cyclically.
    if (isCyclicType(type, node->sym()->type())) {
      cc_.reportError(node->loc(), Message_CannotResolveCyclicType);
      return;
    }
  }

  void visitAssignment(Assignment *node) {
    node->lvalue()->accept(this);
    node->expression()->accept(this);
  }
  void visitBinaryExpression(BinaryExpression *node) {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitReturnStatement(ReturnStatement *node) {
    if (node->expression())
      node->expression()->accept(this);
  }
  void visitForStatement(ForStatement *node) {
    EnterScope scope(&scope_, node->scope());

    if (node->initialization())
      node->initialization()->accept(this);
    if (node->condition())
      node->condition()->accept(this);
    if (node->update())
      node->update()->accept(this);
    node->body()->accept(this);
  }
  void visitBlockStatement(BlockStatement *node) {
    EnterScope scope(&scope_, node->scope());

    for (size_t i = 0; i < node->statements()->length(); i++)
      node->statements()->at(i)->accept(this);
  }
  void visitExpressionStatement(ExpressionStatement *node) {
    node->expression()->accept(this);
  }
  void visitCallExpression(CallExpression *node) {
    node->callee()->accept(this);
    for (size_t i = 0; i < node->arguments()->length(); i++)
      node->arguments()->at(i)->accept(this);
  }
  void visitIfStatement(IfStatement *node) {
    node->condition()->accept(this);
    node->ifTrue()->accept(this);
    if (node->ifFalse())
      node->ifFalse()->accept(this);
  }
  void visitFieldExpression(FieldExpression *node) override {
    node->base()->accept(this);
  }
  void visitIndexExpression(IndexExpression *node) {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitWhileStatement(WhileStatement *node) {
    node->condition()->accept(this);
    node->body()->accept(this);
  }
  void visitIncDecExpression(IncDecExpression *node) {
    node->expression()->accept(this);
  }
  void visitUnaryExpression(UnaryExpression *node) {
    if (node->tag())
      node->tag()->accept(this);
    node->expression()->accept(this);
  }
  void visitTernaryExpression(TernaryExpression *node) {
    node->condition()->accept(this);
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitSwitchStatement(SwitchStatement *node) {
    node->expression()->accept(this);
    if (node->defaultCase())
      node->defaultCase()->accept(this);
    for (size_t i = 0; i < node->cases()->length(); i++) {
      // We don't test case expressions because they are literals.
      Case *c = node->cases()->at(i);
      c->statement()->accept(this);
    }
  }
  void visitArrayLiteral(ArrayLiteral *node) override {
    for (size_t i = 0; i < node->expressions()->length(); i++)
      node->expressions()->at(i)->accept(this);
  }

  // No-op cases.
  void visitFloatLiteral(FloatLiteral *node) override {
  }
  void visitBreakStatement(BreakStatement *node) override {
  }
  void visitContinueStatement(ContinueStatement *node) override {
  }
  void visitStringLiteral(StringLiteral *node) override {
  }
  void visitCharLiteral(CharLiteral *node) override {
  }
  void visitThisExpression(ThisExpression *node) override {
  }
  void visitNameProxy(NameProxy *node) override {
  }
  void visitIntegerLiteral(IntegerLiteral *node) {
  }
  void visitBooleanLiteral(BooleanLiteral *node) {
  }

 private:
  static const int kRankUnvisited;

  // Returns true if it could be resolved to a constant integer; false
  // otherwise. |outp| is unmodified on failure.
  bool resolveConstantArraySize(Expression *expr, int *outp) {
    // We specify Required here, since we will not evaluate the expression
    // otherwise, and we need to report errors.
    BoxedPrimitive value;
    ConstantEvaluator ceval(cc_, scope_, ConstantEvaluator::Required);
    switch (ceval.Evaluate(expr, &value)) {
      case ConstantEvaluator::Ok:
        break;
      case ConstantEvaluator::NotConstant:
        cc_.reportError(expr->loc(), Message_ArraySizeMustBeConstant);
        return false;
      default:
        return false;
    }

    if (!value.isInt()) {
      cc_.reportError(expr->loc(), Message_ArraySizeMustBeInteger);
      return false;
    }
    if (value.toInt() <= 0) {
      cc_.reportError(expr->loc(), Message_ArraySizeMustBePositive);
      return false;
    }

    *outp = value.toInt();
    return true;
  }

  void updateComputedRankSize(Vector<int> &out, size_t rank, int size) {
    if (out[rank] == kRankUnvisited) {
      // If we've never visited this rank before, give it whatever we're inputing
      // as the initial size.
      out[rank] = size;
    } else if (out[rank] >= ArrayType::kUnsized && out[rank] != size) {
      // If we previously detected a fixed size or dynamic size for this rank,
      // and now are receiving a different size, mark the rank size as
      // indeterminate.
      out[rank] = ArrayType::kIndeterminate;
    }
  }

  void computeFixedArrayLiteralDimensions(ArrayLiteral *root, size_t rank, size_t highestUnknownRank, Vector<int> &out) {
    if (rank >= highestUnknownRank) {
      // Either we've reached the end of the [] sequences on the type, or all
      // further ranks are known to have a size.
      return;
    }

    for (size_t i = 0; i < root->expressions()->length(); i++) {
      Expression *expr = root->expressions()->at(i);
      if (ArrayLiteral *lit = expr->asArrayLiteral()) {
        if (lit->isFixedArrayLiteral()) {
          updateComputedRankSize(out, rank, lit->arrayLength());
          computeFixedArrayLiteralDimensions(lit, rank + 1, highestUnknownRank, out);
        } else {
          updateComputedRankSize(out, rank, ArrayType::kUnsized);
        }
      } else if (StringLiteral *lit = expr->asStringLiteral()) {
        updateComputedRankSize(out, rank, lit->arrayLength());
      } else {
        // If we get here, we either have an invalid construction that looks
        // something like:
        //  int a[][] = { 3 };
        //
        // I.e., the user has specified more array dimensions than there are
        // arrays in the literal. We just mark as indeterminate since we'll
        // error for real in the semantic analysis pass.
        updateComputedRankSize(out, rank, ArrayType::kIndeterminate);
      }
    }
  }

  Vector<int> fixedArrayLiteralDimensions(TypeSpecifier *spec, ArrayLiteral *lit) {
    Vector<int> out;

    size_t highestUnknownRank = 0;
    for (size_t i = 0; i < spec->rank(); i++) {
      if (!spec->sizeOfRank(i))
        highestUnknownRank = i + 1;
      out.append(kRankUnvisited);
    }

    if (highestUnknownRank > 0) {
      // Some dimensions were unsized. Compute fixed sizes (if possible) from
      // the initializer. This is a recursive process so we can try to create
      // uniform fixed lengths for each sub-array, as SourcePawn 1 did.
      updateComputedRankSize(out, 0, lit->arrayLength());
      computeFixedArrayLiteralDimensions(lit, 1, highestUnknownRank, out);
    }

    // Return the computed list unadultered - resolveArrayComponentTypes()
    // will correctly merge with the TypeSpecifier.
    return out;
  }

  void resolveTypesInSignature(FunctionSignature *sig) {
    resolveType(sig->returnType());
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      VariableDeclaration *param = sig->parameters()->at(i);
      resolveType(param->spec());
    }
  }

  Type *resolveType(TypeSpecifier *spec, const Vector<int> *arrayInitData = nullptr) {
    if (spec->resolved())
      return spec->resolved();

    Type *baseType = resolveBaseType(spec);
    if (!baseType) {
      // Create a placeholder so we don't have to check null everywhere.
      baseType = TypedefType::New(cc_.add("__unresolved_type__"));
    }

    // Should not have a reference here.
    assert(!baseType->isReference());

    Type *type = baseType;
    if (spec->rank())
      type = resolveArrayComponentTypes(spec, type, arrayInitData);

    if (spec->isByRef()) {
      // We refuse to allow by-ref arrays; arrays are always by-ref. We check
      // this once here, and once again parsing parameters in case we had
      // placeholders.
      //
      // Note: We also forbid this for objects (is ref basically deprecated?
      // I guess so).
      if (type->canUseInReferenceType())
        type = cc_.types()->newReference(type);
      else
        cc_.reportError(spec->byRefLoc(), Message_TypeCannotBeReference, GetTypeClassName(type));
    }

    if (spec->isConst())
      type = cc_.types()->newQualified(type, Qualifiers::Const);

    // If we've wrapped the type, and it was a placeholder, we have to make
    // sure later that the transformation we just made is still valid.
    if (baseType->isUnresolvedTypedef() && baseType != type)
      placeholder_checks_.append(LazyPlaceholderCheck(spec, baseType->toTypedef()));

    spec->setResolved(type);
    return type;
  }

  Type *resolveBaseType(TypeSpecifier *spec) {
    assert(!spec->resolved());

    switch (spec->resolver()) {
      case TOK_LABEL:
      case TOK_NAME:
        return resolveNameToType(spec->proxy());

      case TOK_VOID:
        return cc_.types()->getVoid();
      case TOK_IMPLICIT_INT:
      case TOK_INT:
        return cc_.types()->getPrimitive(PrimitiveType::Int32);
      case TOK_BOOL:
        return cc_.types()->getPrimitive(PrimitiveType::Bool);
      case TOK_CHAR:
        return cc_.types()->getPrimitive(PrimitiveType::Char);
      case TOK_FLOAT:
        return cc_.types()->getPrimitive(PrimitiveType::Float);

      case TOK_FUNCTION:
      {
        FunctionSignature *sig = spec->signature();
        resolveTypesInSignature(sig);

        return FunctionType::New(sig);
      }

      default:
        assert(false);
        return nullptr;
    }
  }

  Type *resolveNameToType(NameProxy *proxy) {
    assert(proxy->sym());

    TypeSymbol *sym = proxy->sym()->asType();
    if (!sym) {
      cc_.reportError(proxy->loc(), Message_IdentifierIsNotAType, proxy->sym()->name()->chars());
      return nullptr;
    }

    if (!sym->type() && sym->node()->asTypedefStatement()) {
      // Create a placeholder. This allows us to resolve dependent types that
      // have not yet been resolved. For example,
      //
      // typedef X function Y ();
      // typedef Y int
      //
      // Here, we'll create a Typedef for Y, which will be filled in later.
      sym->setType(TypedefType::New(sym->name()));
    }

    return sym->type();
  }

  Type *resolveArrayComponentTypes(TypeSpecifier *spec, Type *type, const Vector<int> *arrayInitData = nullptr) {
    if (type->isVoid())
      cc_.reportError(spec->arrayLoc(), Message_CannotCreateArrayOfVoid);

    size_t rank = spec->rank() - 1;
    do {
      int arraySize = ArrayType::kUnsized;
      Expression *expr = spec->sizeOfRank(rank);
      if (expr) {
        resolveConstantArraySize(expr, &arraySize);
      } else if (arrayInitData && rank < arrayInitData->length()) {
        // Use an inferred length if one is available.
        if (arrayInitData->at(rank) != kRankUnvisited)
          arraySize = arrayInitData->at(rank);
      }
      type = cc_.types()->newArray(type, arraySize);
    } while (rank--);
    return type;
  }

  // If traversing the type nesting of |check| reveals a pointer to |first|,
  // we consider the type cyclic and unresolvable.
  bool isCyclicType(Type *check, Type *first) {
    if (check == first || check->canonical() == first)
      return true;

    if (check->isArray())
      return isCyclicType(check->toArray()->contained(), first);
    if (check->isResolvedTypedef())
      return isCyclicType(check->toTypedef()->actual(), first);
    if (check->isReference())
      return isCyclicType(check->toReference()->contained(), first);
    if (check->isFunction()) {
      FunctionSignature *signature = check->toFunction()->signature();
      if (isCyclicType(signature->returnType()->resolved(), first))
        return true;
      for (size_t i = 0; i < signature->parameters()->length(); i++) {
        VariableDeclaration *decl = signature->parameters()->at(i);
        if (isCyclicType(decl->spec()->resolved(), first))
          return true;
      }
    }

    return false;
  }

 private:
  PoolAllocator &pool_;
  CompileContext &cc_;
  TranslationUnit *unit_;
  FunctionNode *fun_;
  Scope *scope_;

  struct LazyPlaceholderCheck {
    LazyPlaceholderCheck(TypeSpecifier *spec, TypedefType *placeholder)
      : spec(spec),
        placeholder(placeholder)
    {}
    TypeSpecifier *spec;
    TypedefType *placeholder;
  };
  Vector<LazyPlaceholderCheck> placeholder_checks_;
};

bool
ke::ResolveTypes(CompileContext &cc, TranslationUnit *unit)
{
  TypeResolver binder(cc, unit);
  if (!binder.analyze())
    return false;

  return cc.nerrors() == 0;
}

const int TypeResolver::kRankUnvisited = INT_MIN;
