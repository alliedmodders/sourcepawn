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
#include "layout.h"

using namespace ke;
using namespace sp;

template <typename T>
class AutoPush
{
 public:
  AutoPush(Vector<T> &stack, const T &item)
   : stack_(stack)
  {
    stack_.append(item);
  }
  ~AutoPush() {
    stack_.pop();
  }
 private:
  Vector<T> &stack_;
};

// Type Resolution ensures that any type specifier has a bound type, and that
// any constant expression has a constant value.
//
// This pass involves a recursive walk through the AST. Unlike other passes,
// it may recursively resolve other unrelated AST nodes. For example,
// 
//   typedef A = B;
//   typedef B = int;
//
// In order to resolve "A", we will recursively resolve "B". If we have a
// recursive type, we have to prevent infintie recursion and report an error.
// Recursive types always occur through name resolution, and there are many
// patterns in which they can occur.
//
// ---- CONSTANT RECURSION ----
//
// The first form of recursion we're concerned about is 'constant recursion'.
// This occurs when resolving a constant expression, it is mutually dependent
// on another constant expression. There are a few ways to do this. The first
// is via enum values:
//
//    1: enum X {
//    2:   A = B,
//    3:   B,
//    4: };
//
// On line 2, resolving the type of 'A' depends on resolving 'B'. We cannot
// resolve 'B' without knowing the type of 'A'.
//
// Another form of constant recursion is through constant definitions:
//
//    1: const int A = B;
//    2: const int B = A;
//
// ---- TYPE RECURSION ----
//
// We classify type recursion in two forms. The first is simple type recursion,
// when a typedef refers to itself. These cycles are easy to break, and we
// break them in visitTypedef() via a placeholder type. An example:
//
//    typedef A = B
//    typedef B = A
//
// While visit "A", we will create a blank TypedefType object. We will then
// visit B, which will build a TypedefType object wrapping A. Once back in A,
// we do a quick check that A does not depend on itself for computing a
// canonical type.
//
// The other form of type recursive is size-dependent types. An easy example of
// this in C would be:
//
//    struct X {
//      X x;
//    };
//
// To allocate a struct we must be able to compute its size. But here, its size
// infinitely expands, and so the type is not resolveable. Another case where
// this can happen is with the `sizeof` constexpr. For example,
//
//    int X[sizeof(Y)] = 10;
//    int Y[sizeof(X)] = 20;
//
// Here, the size of `x` is dependent on computing its own size. We will break
// this cycle when calling resolveType for sizeof(X), since we will not have
// finished resolving the type of X. In the future, we may be able to reach
// something like this through typedefs as well:
//
//    typedef X = int[sizeof(Y)];
//    typedef Y = int[sizeof(X)];
//
// This cycle would be broken by sizeof() itself, since it checks whether or
// not it is trying to compute an unresolved type.
//
// Types can generally reference themselves as long as they do not create size
// dependencies, and in the case of typedefs, as long as we can resolve them to
// a canonical type that is not a typedef.
//
class TypeResolver
 : public AstVisitor,
   public ConstantResolver
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

      if (!cc_.canContinueProcessing())
        return false;
    }

    return true;
  }

  // Override for ConstantEvaluator. For simplicity, and to prevent extra
  // errors, we always give ConstantEvaluator a type pointer back.
  Type *resolveTypeOfVar(VariableSymbol *sym) override {
    if (!sym->type())
      sym->node()->accept(this);
    return sym->type();
  }

  virtual bool resolveVarAsConstant(VariableSymbol *sym, BoxedValue *out) {
    // If we don't have a type, resolve the node right now. This will also
    // resolve any constexpr, if any.
    if (!sym->type()) {
      sym->node()->accept(this);
      assert(sym->hasCheckedForConstExpr());
    }

    // We have a type, so we know whether it can be used as a constant.
    if (!sym->canUseInConstExpr())
      return false;

    // Technically, we must have a constexpr by now. Sometime later we might
    // let local constants be initialized with non-const values, so we check
    // here.
    if (!sym->isConstExpr())
      return false;

    *out = sym->constExpr();
    return true;
  }

  // Override for ConstantEvaluator. If a constant fails to resolve, it must
  // be set to contain a bogus value (preferably 0 in whatever type is needed).
  BoxedValue resolveValueOfConstant(ConstantSymbol *sym) override {
    if (!sym->hasValue())
      resolveConstant(sym);
    
    return sym->value();
  }

  void visitVariableDeclaration(VariableDeclaration *node) override {
    VariableSymbol *sym = node->sym();

    if (!sym->type()) {
      if (TypeSpecifier *spec = node->te().spec()) {
        // Note: we should not be able to recurse from inside this block. If it
        // could, we'd have to mark spec as resolving earlier.
        Vector<int> literal_dims;
        if (Expression *init = node->initialization()) {
          if (spec->hasPostDims()) {
            // Compute the dimensions of initializers in case the declaration type
            // requires inference.
            if (ArrayLiteral *lit = init->asArrayLiteral()) {
              literal_dims = fixedArrayLiteralDimensions(spec, lit);
            } else if (StringLiteral *lit = init->asStringLiteral()) {
              literal_dims.append(lit->arrayLength());
            }
          }
        }

        sym->setType(resolveType(node->te(), &literal_dims));
      } else {
        sym->setType(node->te().resolved());
      }
    }

    if (sym->isConstExpr() || !sym->canUseInConstExpr())
      return;

    // If we're currently trying to resolve this variable's constant
    // expression, report an error.
    if (sym->isResolvingConstExpr()) {
      cc_.report(node->loc(), rmsg::recursive_constexpr)
        << sym->name();

      // Pawn requires that const variables have constexprs, so we just set a
      // default one to quell as many other errors as we can. In the future we
      // may want to lax this restriction.
      sym->setConstExpr(DefaultValueForPlainType(sym->type()));
      return;
    }

    // We got a constexpr with no initialization. Just assume it's 0, but
    // report an error as SP1 does.
    if (!node->initialization()) {
      cc_.report(node->loc(), rmsg::constant_var_needs_constexpr)
        << sym->name();
      sym->setConstExpr(DefaultValueForPlainType(sym->type()));
      return;
    }

    sym->setResolvingConstExpr();

    // In Pawn, a const var *must* be a constexpr. We only care about this for
    // ints/floats since constexprs aren't really relevant yet otherwise.
    BoxedValue box;
    ConstantEvaluator ceval(cc_, this, ConstantEvaluator::Required);
    switch (ceval.Evaluate(node->initialization(), &box)) {
      case ConstantEvaluator::Ok:
        break;
      case ConstantEvaluator::NotConstant:
        cc_.report(node->loc(), rmsg::constant_var_needs_constexpr)
          << sym->name();
        // FALLTHROUGH.
      case ConstantEvaluator::TypeError:
        // Error has already been reported.
        box = DefaultValueForPlainType(sym->type());
        break;
      default:
        assert(false);
    }

    // :TODO: type check

    sym->setConstExpr(box);
  }

  void visitEnumConstant(EnumConstant *node) override {
    Type *type = node->parent()->sym()->type();

    // If we don't have an initializer, or our parent type hasn't been
    // instantiated yet, we upcall to resolve our parent Enum instead.
    //
    // We could potentially re-enter, but only after the parent has a
    // type, so we'll take a different path next time.
    if (!type || !node->expression()) {
      // There is no way we can resolve this node without resolving everything
      // in the enum. However, if the enum is already being resolved, this
      // means we have a circular dependency between two enum values. Error
      // about that now.
      if (type) {
        // We can reach this through "enum A { B = C, C };" if we are resolving
        // the enum through the normal AST walk.
        cc_.report(node->loc(), rmsg::enum_depends_on_child_enum)
          << enum_constant_stack_.back()->name()
          << node->name()
          << cc_.note(enum_constant_stack_.back()->loc(), rmsg::here);

        // Always set a value.
        node->sym()->setTypeAndValue(type, DefaultValueForPlainType(type));
        return;
      }
      
      // We just up-call to our parent, since it needs to resolve all of the
      // enum values in-order anyway.
      node->parent()->accept(this);
    } else {
      // Note: on failure we just put a dummy 0 in place, so we don't create
      // extra NotConstant errors.
      int value;
      if (!resolveEnumConstantValue(node, &value))
        value = 0;
      node->sym()->setTypeAndValue(type, BoxedValue(type, IntValue::FromInt32(value)));
    }
  }

  void visitEnumStatement(EnumStatement *node) override {
    // This should only happen if we resolved the enum before visiting it in
    // the statement list.
    if (node->sym()->type())
      return;

    Type *type;
    if (node->name()) {
      type = cc_.types()->newEnum(node->name());
    } else {
      type = cc_.types()->getPrimitive(PrimitiveType::Int32);
    }
    node->sym()->setType(type);

    int value = 0;
    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumConstant *cs = node->entries()->at(i);
      if (cs->expression()) {
        // We may have already resolved a value, for example:
        //    enum X {
        //       Y = Z,
        //       Z = 3,
        //    };
        //
        // Surprisingly, we can resolve "Z" before "Y" with our type resolution
        // algorithm.
        if (!cs->sym()->hasValue()) {
          if (!resolveEnumConstantValue(cs, &value))
            value = 0;
        } else {
          // The value should already have resolved as an int.
          value = cs->sym()->value().toInteger().asInt32();
        }
      }

      cs->sym()->setTypeAndValue(type, BoxedValue(type, IntValue::FromInt32(value)));
      value++;
    }
  }

  void visitStructInitializer(StructInitializer *node) override {
    for (size_t i = 0; i < node->pairs()->length(); i++) {
      Expression *expr = node->pairs()->at(i)->expr();
      expr->accept(this);
    }
  }

 private:
  EnumType *resolveMethodmapParentType(NameProxy *proxy) {
    // The parent must be a methodmap.
    TypeSymbol *parentSymbol = proxy->sym()->asType();
    if (!parentSymbol) {
      cc_.report(proxy->loc(), rmsg::bad_methodmap_parent)
        << proxy->name();
      return nullptr;
    }

    // Don't error twice if we get an unresolved type.
    Type *type = resolveNameToType(proxy);
    if (type->isUnresolvable())
      return nullptr;

    if (!type->isEnum() || !type->toEnum()->methodmap()) {
      cc_.report(proxy->loc(), rmsg::bad_methodmap_parent_type)
        << type;
      return nullptr;
    }

    return type->toEnum();
  }

 public:
  void visitMethodmapDecl(MethodmapDecl *methodmap) override {
    EnumType *type = nullptr;

    // We can tell whether or not the methodmap is attached to an enum by the
    // node that created its TypeSymbol. If it's not the same, the symbol's
    // node is the enum we're attached to.
    if (methodmap->sym()->node() == methodmap) {
      // If we've already got a type, this methodmap has already been resolved.
      if (methodmap->sym()->type()) {
        assert(methodmap->sym()->type()->isEnum());
        return;
      }

      // Create a new enum type.
      type = cc_.types()->newEnum(methodmap->name());
      methodmap->sym()->setType(type);
    } else {
      // It's impossible to get here without already having a resolved enum.
      // Name bindings prevent defining enums *after* methodmaps, and we always
      // resolve enum methodmaps as part of resolving the enum. The methodmap
      // decl is not pointed to by anything else.
      type = methodmap->sym()->type()->toEnum();

      // Check to see whether we've already resolved this methodmap.
      if (type->methodmap())
        return;
    }

    // Create a methodmap now so we don't re-enter.
    Methodmap *mm = new (pool_) Methodmap();
    type->setMethodmap(mm);

    // Our parent must be resolved first.
    EnumType *parent = nullptr;
    if (methodmap->parent()) {
      methodmap->parent()->accept(this);
      parent = resolveMethodmapParentType(methodmap->parent());
    }

    // Check that we do not appear in the parent chain.
    for (EnumType *cursor = parent; cursor; cursor = cursor->methodmap()->parent()) {
      if (cursor->methodmap() == mm) {
        cc_.report(methodmap->parent()->loc(), rmsg::circular_methodmap)
          << methodmap->name();
        parent = nullptr;
        break;
      }
    }
    mm->setParent(parent);

    for (size_t i = 0; i < methodmap->body()->length(); i++) {
      LayoutDecl *decl = methodmap->body()->at(i);
      decl->accept(this);
    }
  }

  void visitRecordDecl(RecordDecl *layout) override {
    // Check whether we've already resolved this somewhere else.
    if (layout->sym()->type())
      return;

    RecordType *type = nullptr;
    switch (layout->token()) {
      case TOK_UNION:
        type = cc_.types()->newUnion(layout->name());
        break;

      case TOK_STRUCT:
        type = cc_.types()->newStruct(layout->name());
        break;

      default:
        assert(false);
    }

    layout->sym()->setType(type);

    for (size_t i = 0; i < layout->body()->length(); i++) {
      LayoutDecl *decl = layout->body()->at(i);
      decl->accept(this);
    }
  }

 private:
  void visitFunction(FunctionNode *node) {
    resolveTypesInSignature(node->signature());

    SaveAndSet<FunctionNode *> save(&fun_, node);
    EnterScope funScope(&scope_, node->funScope());
    EnterScope varScope(&scope_, node->varScope());

    if (node->body())
      node->body()->accept(this);
  }

 public:
  void visitFieldDecl(FieldDecl *decl) override {
    // It is not possible to refer to fields as part of a constant or type
    // expression - no re-entrancy or upward-resolving needed.
    //
    // Eventually this will change because of statics and inner typedefs, and
    // we'll have to make this look similar to visitEnumConstant.
    assert(!decl->sym() || !decl->sym()->type());

    // Note that fields can be anonymous if in a block union.
    Type *type = resolveType(decl->te());
    if (decl->sym())
      decl->sym()->setType(type);
  }

  void visitPropertyDecl(PropertyDecl *decl) override {
    // It is impossible to use property decl types in constant exprs or
    // type resolvers. I don't anticipate this ever being a thing  either,
    // unless we get some kind of decltype() operator. In any case if that
    // happens then this will need to look more like visitEnumConstant and
    // have re-entrancy guards.
    assert(!decl->sym()->type());

    decl->sym()->setType(resolveType(decl->te()));

    if (decl->getter()->isFunction())
      visitFunction(decl->getter()->fun());
    if (decl->setter()->isFunction())
      visitFunction(decl->setter()->fun());
  }

  void visitMethodDecl(MethodDecl *decl) override {
    // It is not possible to refer to methods as part of a constant or type
    // expression - no re-entrancy or upward-resolving needed.
    if (decl->method()->isFunction())
      visitFunction(decl->method()->fun());
  }

 public:
  void visitFunctionStatement(FunctionStatement *node) override {
    visitFunction(node);
  }

  void visitTypedefStatement(TypedefStatement *node) override {
    // Recursion guard.
    if (node->te().resolved())
      return;

    // Create a placeholder until we've resolved the typedef.
    TypedefType *type = cc_.types()->newTypedef(node->name());
    node->sym()->setType(type);

    const SourceLocation &baseLoc = node->te().spec()->baseLoc();
    Type *actual = resolveType(node->te());
    
    // Check for a recursive type. There might be a better way to do this, but
    // for now this is what we've got: just manually check any compound type
    // that wouldn't be able to handle a self-reference.
    Type *base = actual;
    if (ArrayType *array = base->asArray()) {
      while (array->contained()->isArray())
        array = array->contained()->toArray();
      base = array->contained();
    } else if (ReferenceType *ref = base->asReference()) {
      base = ref->contained();
    }

    if (type == base->canonical()) {
      type->resolve(&UnresolvableType);
      cc_.report(baseLoc, rmsg::recursive_type);
      return;
    }

    type->resolve(actual);
  }

  // These cases do not define types, but we do have to visit their children
  // to find nested declarations or functions.
  void visitAssignment(Assignment *node) override {
    node->lvalue()->accept(this);
    node->expression()->accept(this);
  }
  void visitBinaryExpression(BinaryExpression *node) override {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitReturnStatement(ReturnStatement *node) override {
    if (node->expression())
      node->expression()->accept(this);
  }
  void visitDeleteStatement(DeleteStatement *node) override {
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
  void visitTokenLiteral(TokenLiteral *node) {
  }
  void visitSizeofExpression(SizeofExpression *node) override {
  }

 private:
  static const int kRankUnvisited;

  // Returns true if it could be resolved to a constant integer; false
  // otherwise. |outp| is unmodified on failure.
  bool resolveConstantArraySize(TypeSpecifier *spec, Expression *expr, int *outp) {
    // With old-style decls, we have to speculatively parse for constants.
    ConstantEvaluator::Mode mode = spec->isOldDecl()
      ? ConstantEvaluator::Speculative
      : ConstantEvaluator::Required;
    ConstantEvaluator ceval(cc_, this, mode);

    BoxedValue value;
    switch (ceval.Evaluate(expr, &value)) {
      case ConstantEvaluator::Ok:
        break;
      case ConstantEvaluator::NotConstant:
        if (mode == ConstantEvaluator::Required)
          cc_.report(expr->loc(), rmsg::array_size_must_be_constant);
        return false;
      case ConstantEvaluator::TypeError:
        // Error already reported.
        return false;
      default:
        assert(false);
        return false;
    }

    if (!value.isInteger()) {
      cc_.report(expr->loc(), rmsg::array_size_must_be_int);
      return false;
    }

    const IntValue &iv = value.toInteger();
    if (iv.isNegativeOrZero()) {
      cc_.report(expr->loc(), rmsg::array_size_must_be_positive);
      return false;
    }
    if (!iv.valueFitsInInt32()) {
      cc_.report(expr->loc(), rmsg::array_size_too_large);
      return false;
    }

    *outp = iv.asInt32();
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
      resolveTypeIfNeeded(param->te());
    }
  }

  // The main entrypoint for resolving ConstantSymbols.
  void resolveConstant(ConstantSymbol *sym) {
    // Since we're only called from resolveValueOfConstant, we should never
    // arrive here with a constant already set.
    assert(!sym->hasValue());

    if (sym->isResolving()) {
      cc_.report(sym->node()->loc(), rmsg::recursive_constexpr)
        << sym->name();
      return;
    }

    // The resolver (such as visitEnumConstant) is responsible for setting the
    // constant value.
    sym->setResolving();
    sym->node()->accept(this);
    assert(sym->hasValue());
  }

  Type *resolveTypeIfNeeded(TypeExpr &te) {
    if (te.resolved())
      return te.resolved();
    return resolveType(te);
  }

  Type *resolveType(TypeExpr &te, const Vector<int> *arrayInitData = nullptr) {
    if (te.resolved())
      return te.resolved();

    TypeSpecifier *spec = te.spec();
    if (spec->isResolving()) {
      cc_.report(spec->baseLoc(), rmsg::recursive_type);

      // We don't want to report this twice, so mark it as resolved.
      te.setResolved(&UnresolvableType);
      return te.resolved();
    }

    spec->setResolving();

    Type *baseType = resolveBaseType(spec);
    if (!baseType) {
      // Return a placeholder so we don't have to check null everywhere.
      te.setResolved(&UnresolvableType);
      return te.resolved();
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
      if (type->canUseInReferenceType()) {
        type = cc_.types()->newReference(type);
      } else {
        cc_.report(spec->byRefLoc(), rmsg::type_cannot_be_ref)
          << type;
      }
    }

    if (spec->isConst())
      type = cc_.types()->newQualified(type, Qualifiers::Const);

    // If we already had a type here, we must have seen a recursive type. It's
    // okay to rewrite it since we already reported an error somewhere.
    assert(!te.resolved() || te.resolved()->isUnresolvable());

    te.setResolved(type);
    return type;
  }

  Type *resolveBaseType(TypeSpecifier *spec) {
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
      // It's okay to return null here - our caller will massage it into
      // UnresolvableType.
      cc_.report(proxy->loc(), rmsg::not_a_type)
        << proxy->sym()->name();
      return nullptr;
    }

    if (!sym->type()) {
      // See the giant comment at the top of this file about type recursion.
      // We should not recurse through here since anything that can construct
      // a TypeSymbol should either immediately set a type, or be dependent
      // on TypeSpecifier's recursion checking.
      //
      // As of a 11/27/2014 refactoring, all AST nodes that can define types,
      // including typedefs, fill in an empty Type object to prevent recursion.
      // Each data structure may have its own special cases for the types of 
      // recursion it can/cannot support.
      sym->node()->accept(this);

      // We should always get a type. Even if everything fails, we should have
      // set an UnresolvableType.
      assert(sym->type());
    }

    return sym->type();
  }

  Type *resolveArrayComponentTypes(TypeSpecifier *spec, Type *type, const Vector<int> *arrayInitData = nullptr) {
    if (type->isVoid())
      cc_.report(spec->arrayLoc(), rmsg::array_of_void);

    size_t rank = spec->rank() - 1;
    do {
      int arraySize = ArrayType::kUnsized;
      Expression *expr = spec->sizeOfRank(rank);
      if (expr) {
        // Should either be old-style, or fixed-array style.
        assert(spec->isOldDecl() || spec->hasPostDims());

        resolveConstantArraySize(spec, expr, &arraySize);
      } else if (arrayInitData && rank < arrayInitData->length()) {
        // Should either be old-style, or fixed-array style.
        assert(spec->isOldDecl() || spec->hasPostDims());

        // Use an inferred length if one is available.
        if (arrayInitData->at(rank) != kRankUnvisited)
          arraySize = arrayInitData->at(rank);
      }
      type = cc_.types()->newArray(type, arraySize);
    } while (rank--);
    return type;
  }

  bool resolveEnumConstantValue(EnumConstant *cs, int *outp) {
    AutoPush<EnumConstant *> track(enum_constant_stack_, cs);

    BoxedValue out;
    ConstantEvaluator ceval(cc_, this, ConstantEvaluator::Required);
    switch (ceval.Evaluate(cs->expression(), &out)) {
      case ConstantEvaluator::Ok:
        if (!out.isInteger() || !out.toInteger().typeFitsInInt32()) {
          cc_.report(cs->expression()->loc(), rmsg::enum_value_must_be_int);
          return false;
        }
        break;
      case ConstantEvaluator::NotConstant:
        cc_.report(cs->expression()->loc(), rmsg::enum_value_must_be_constant);
        return false;
      case ConstantEvaluator::TypeError:
        // error already reported.
        return false;
      default:
        // :TODO: assert unreached instead.
        assert(false);
        return false;
    }

    // Note that we don't check the actual type attached to the box here.
    // Neither C++ nor Pawn do, not that that is necessarily a good reason.
    // This means the following code is legal:
    //
    // enum A {
    //   B
    // };
    // enum C {
    //   D = B
    // };
    //
    // Even though B's type is A, not C.
    *outp = out.toInteger().asInt32();
    return true;
  }

 private:
  PoolAllocator &pool_;
  CompileContext &cc_;
  TranslationUnit *unit_;
  FunctionNode *fun_;
  Scope *scope_;

  Vector<EnumConstant *> enum_constant_stack_;
};

const int TypeResolver::kRankUnvisited = INT_MIN;

bool
sp::ResolveTypes(CompileContext &cc, TranslationUnit *unit)
{
  TypeResolver binder(cc, unit);
  if (!binder.analyze())
    return false;

  return cc.phasePassed();
}
