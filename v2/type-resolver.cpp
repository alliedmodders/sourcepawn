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

static const int INFER_ARRAY_SIZE = 0;
static const int EVAL_ARRAY_SIZE = -1;
static const int DYNAMIC_ARRAY_SIZE = -2;

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
#if 0
    // Bind the initializer before registering the declaration, so that we
    // can error on bogus initializers (new x = x).
    if (node->initialization())
      node->initialization()->accept(this);

    Type *type = bindType(node->type());
    if (!type)
      return;

    if (node->dims() || type->isArray()) {
      int dims[MAX_ARRAY_DEPTH];
      int levels = evaluateDimensions(node->loc(), type, node->dims(), dims);
      if (levels < 0)
        return;

      // Do some extra inference based on the initializer, if present.
      Expression *init = node->initialization();
      if (init &&
        init->isArrayLiteral() &&
        init->toArrayLiteral()->isFixed())
      {
        Type *contained = type->isArray()
                          ? type->toArray()
                          : type;
        if (!inferFixedArrayDimensions(init->toArrayLiteral(), contained, dims, levels))
          return;
      }

      // If we got extra dimensions, we need to build a new type.
      if (node->dims()) {
        if ((type = buildArrayType(type, dims, node->dims()->length())) == nullptr)
          return;
      }
    }

    // If the node already has a symbol (meaning it was a global), then
    // we don't have to do anything more.
    if (node->sym()) {
      assert(node->sym()->scope()->kind() == Scope::GLOBAL);
      node->sym()->setType(type);
      return;
    }

    VariableSymbol *sym = new (pool_) VariableSymbol(link_->scope(), node->name(), node->loc(), type);
    node->setSymbol(sym);
     
    if (Symbol *other = link_->scope()->localLookup(sym->name())) {
      // Report, but allow errors to continue.
      cc_.reportError(sym->loc(), Message_RedeclaredName,
              sym->name()->chars(),
              other->loc().pos.line,
              other->loc().pos.col);
      return;
    }
    
    if (!link_->scope()->addSymbol(sym))
      return;
#endif
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

  void visitTypedefStatement(TypedefStatement *tdef) {
    Type *type = resolveType(tdef->spec());
    if (!type) {
      // Even if we couldn't resolve, put a placeholder there so we can keep
      // trying to bind names.
      if (!tdef->sym()->type())
        tdef->sym()->setType(TypedefType::New(tdef->sym()->name()));
      return;
    }

    if (tdef->sym()->type()) {
      // We already resolved this type earlier. We have to make sure now that it
      // does not resolve cyclically.
      if (isCyclicType(type, tdef->sym()->type())) {
        cc_.reportError(tdef->loc(), Message_CannotResolveCyclicType);
        return;
      }
      tdef->sym()->type()->toTypedef()->resolve(type);
    }
    tdef->sym()->setType(type);
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
  void visitIntegerLiteral(IntegerLiteral *node) {
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
  void visitBooleanLiteral(BooleanLiteral *node) {
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

 private:
  void resolveTypesInSignature(FunctionSignature *sig) {
    resolveType(sig->returnType());
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      VariableDeclaration *param = sig->parameters()->at(i);
      resolveType(param->spec());
    }
  }

  Type *resolveNameToSymbol(TypeSymbol *sym) {
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

  Type *resolveNameToType(NameProxy *proxy) {
    // Resolve the name.
    visitNameProxy(proxy);
    if (!proxy->sym())
      return nullptr;

    TypeSymbol *sym = proxy->sym()->asType();
    if (!sym) {
      cc_.reportError(proxy->loc(), Message_IdentifierIsNotAType, proxy->sym()->name()->chars());
      return nullptr;
    }
    return resolveNameToSymbol(sym);
  }

  Type *resolveBaseType(TypeSpecifier *spec) {
    assert(!spec->resolved());

    switch (spec->resolver()) {
      case TOK_LABEL:
      case TOK_NAME:
        return resolveNameToType(spec->name());

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

  Type *resolveArrayComponentTypes(TypeSpecifier *spec, Type *type) {
    if (type->isVoid())
      cc_.reportError(spec->arrayLoc(), Message_CannotCreateArrayOfVoid);

    size_t rank = spec->rank() - 1;
    do {
      int arraySize = ArrayType::kUnsized;
      Expression *expr = spec->sizeOfRank(rank);
      if (expr)
        resolveConstantArraySize(expr, &arraySize);
      type = cc_.types()->newArray(type, arraySize);
    } while (rank--);
    return type;
  }

  Type *resolveType(TypeSpecifier *spec) {
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
      type = resolveArrayComponentTypes(spec, type);

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
        VariableSymbol *sym = signature->parameters()->at(i)->sym();
        if (isCyclicType(sym->type(), first))
          return true;
      }
    }

    return false;
  }

#if 0
  int evaluateDimensions(const SourceLocation &loc, Type *typeArg, ExpressionList *exprs, int dims[MAX_ARRAY_DEPTH]) {
    unsigned level = 0;
    Type *type = typeArg;
    while (type->isArray()) {
      ArrayType *atype = type->toArray();
      dims[level] = atype->isFixedLength() ? atype->fixedLength() : DYNAMIC_ARRAY_SIZE;
      type = atype->contained();
      level++;
    }

    if (!exprs)
      return level;

    if (level + exprs->length() > MAX_ARRAY_DEPTH) {
      cc_.reportError(loc, Message_MaximumArrayDepth);
      return -1;
    }

    bool hadEmptySize = false;
    for (size_t i = 0; i < exprs->length(); i++) {
      Expression *expr = exprs->at(i);
      if (!expr) {
        dims[i] = INFER_ARRAY_SIZE;
        hadEmptySize = true;
        continue;
      }

      BoxedPrimitive box;
      ConstantEvaluator ceval(cc_, link_->scope(), ConstantEvaluator::Speculative);
      if (ceval.Evaluate(expr, &box) == ConstantEvaluator::Ok && box.type() == PrimitiveType::Int32) {
        int size = box.toInt();
        if (size <= 0) {
          cc_.reportError(expr->loc(), Message_BadArraySize);
          return false;
        }

        dims[i] = size;
        continue;
      }

      if (hadEmptySize) {
        cc_.reportError(expr->loc(), Message_BadDynamicInitializer);
        return false;
      }

      dims[i] = EVAL_ARRAY_SIZE;
    }

    return level + exprs->length();
  }

  // This function computes missing dimension sizes based on a literal
  // initializer. We don't really handle any size rules here; we just
  // do a really dumb inference.
  bool inferFixedArrayDimensions(ArrayLiteral *lit, Type *contained, int dims[MAX_ARRAY_DEPTH], int levels) {
    for (int i = 0; i < levels; i++) {
      if (dims[i] == EVAL_ARRAY_SIZE || dims[i] == DYNAMIC_ARRAY_SIZE) {
        // The following are invalid:
        //   new x[<expr>] = {...}
        //   new Type:x = {...}  // where Type is a non-fixed array type
        cc_.reportError(lit->loc(), Message_CannotInitializeEvalArray);
        return false;
      }

      if (dims[i] == INFER_ARRAY_SIZE) {
        // Character arrays are always dynamic when inferred from string literals.
        if (i == levels - 1 && contained->isChar())
          dims[i] = DYNAMIC_ARRAY_SIZE;
        else
          dims[i] = lit->expressions()->length();
      }

      if (i != levels - 1) {
        // Fixed array literals are guaranteed to have at least one expression.
        Expression *next = lit->expressions()->at(0);
        if (!next->isArrayLiteral()) {
          // Whelp... this array is just malformed. Not the best error message
          // but whatever.
          cc_.reportError(next->loc(), Message_ArraySizeCannotBeDetermined);
          return false;
        }
        lit = next->toArrayLiteral();
      }
    }

    return true;
  }

  ArrayType *buildArrayType(Type *contained, int dims[MAX_ARRAY_DEPTH], size_t postlevels) {
    Type *type = contained;
    for (size_t i = postlevels - 1; i < postlevels; i--) {
      int size = dims[i] > 0 ? dims[i] : ArrayType::DYNAMIC_ARRAY_SIZE;
      if ((type = cc_.types()->newArray(contained, size)) == nullptr)
        return nullptr;
    }
    return type->toArray();
  }
#endif

  bool fillFunctionType(FunctionType *fun, const FunctionSignature &sig) {
#if 0
    Type *type = nullptr;
    if (!fun->returnType()) {
      if ((type = bindType(sig.returnType())) == nullptr)
        return false;
      fun->setReturnType(type);
    }
#endif

#if 0
    // :TODO: some kind of length check...
    TypeList *params = new (pool_) TypeList(sig.parameters()->length());
    for (size_t i = 0; i < sig.parameters()->length(); i++) {
      Parameter *param = sig.parameters()->at(i);
      if ((type = bindType(param->type())) == nullptr)
        return false;
#endif

#if 0
      if (type->isArray() || param->dims()) {
        int dims[MAX_ARRAY_DEPTH];
        int levels = evaluateDimensions(param->loc(), type, param->dims(), dims);
        if (levels < 0)
          return false;

        // If we got extra dimensions, we need to build a new type.
        // Unlike variables, we do not infer anything about array
        // sizes from its default initializer.
        if (param->dims()) {
          if ((type = buildArrayType(type, dims, param->dims()->length())) == nullptr)
            return false;
        }
        assert(!param->reference());
      } else {
        if (param->reference()) {
          if ((type = cc_.types()->newReference(type)) == nullptr)
            return false;
        }
      }

      params->set(i, type);
      param->sym()->setType(type);
    }

    fun->setParameterTypes(params);
#endif
    return true;
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
