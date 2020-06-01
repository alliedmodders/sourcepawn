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
#include <algorithm>

#include "type-resolver.h"
#include "compile-context.h"
#include "compile-phases.h"

namespace sp {

using namespace ke;
using namespace ast;;

template <typename T>
class AutoPush
{
 public:
  AutoPush(std::vector<T>& stack, const T& item)
   : stack_(stack)
  {
    stack_.push_back(item);
  }
  ~AutoPush() {
    stack_.pop_back();
  }
 private:
  std::vector<T>& stack_;
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
// recursive type, we have to prevent infinite recursion and report an error.
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
TypeResolver::TypeResolver(CompileContext& cc)
 : pool_(cc.pool()),
   cc_(cc)
{
}

bool
TypeResolver::analyze()
{
  printf("unresolved type queue size: %d\n", int(work_queue_.size()));
  while (!work_queue_.empty()) {
    AstNode* node = PopFront(&work_queue_);
    node->accept(this);

    if (!cc_.canContinueProcessing())
      return false;
  }

  return true;
}

// Override for ConstantEvaluator. For simplicity, and to prevent extra
// errors, we always give ConstantEvaluator a type pointer back.
Type*
TypeResolver::resolveTypeOfVar(VariableSymbol* sym)
{
  if (!sym->type())
    sym->node()->accept(this);
  return sym->type();
}

bool
TypeResolver::resolveVarAsConstant(VariableSymbol* sym, BoxedValue* out)
{
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
BoxedValue
TypeResolver::resolveValueOfConstant(ConstantSymbol* sym)
{
  if (!sym->hasValue())
    resolveConstant(sym);
  
  return sym->value();
}

void
TypeResolver::visitVarDecl(VarDecl* node)
{
  VariableSymbol* sym = node->sym();

  if (sym->type() && !sym->canUseInConstExpr()) {
    // This was already resolved earlier - it's part of a function signature.
    assert(sym->scope()->kind() == Scope::Argument);
    return;
  }

  Type* type;
  if (TypeSpecifier* spec = node->te().spec()) {
    // We always infer sizes for postdims in variable scope. In argument
    // scope, we don't want something like:
    //
    //    f(x[] = {}), or
    //
    // To infer as int[0]. However, this should be illegal:
    //
    //    f(int x[] = {})
    //
    // So we simply never infer dimensions for arguments.
    //
    // Note: we should not be able to recurse from inside this block. If it
    // could, we'd have to mark spec as resolving earlier.
    Expression* initializer = nullptr;
    if (!sym->isArgument() && spec->hasPostDims())
      initializer = node->initialization();

    VarDeclSpecHelper helper(node, initializer);
    type = resolveType(node->te(), &helper);

    // If the array was an old-school declaration, like:
    //   decl blah[expr];
    //
    // Then we need to preserve the dimensions from the type, and propagate
    // them to an initializer. This is really out of scope for type resolution,
    // but it's truly the only place we can do this (unless we always stash
    // the dimensions in the AST node).
    if (type->isArray() &&
        !type->toArray()->hasFixedLength() &&
        sym->scope()->kind() == Scope::Block &&
        spec->dims() &&
        !initializer)
    {
      Type* innermost = type->toArray()->innermost();
      initializer = new (pool_) NewArrayExpr(
        spec->arrayLoc(),
        TypeExpr(innermost),
        spec->dims());
      node->set_initializer(initializer);
    }
  } else {
    type = node->te().resolved();
  }

  if (!sym->type() && !assignTypeToSymbol(sym, type))
    return;

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

  // :TODO: type check box

  sym->setConstExpr(box);
}

void
TypeResolver::visitEnumConstant(EnumConstant* node)
{
  if (node->sym()->hasValue())
    return;

  EnumStatement* parent = node->parent();
  Type* type = node->parent()->sym()->type();

  // If we don't have an initializer, or our parent type hasn't been
  // instantiated yet, we upcall to resolve our parent Enum instead.
  //
  // We could potentially re-enter, but only after the parent has a
  // type, so we'll take a different path next time.
  if (!parent->isResolved() || !node->expression()) {
    // There is no way we can resolve this node without resolving everything
    // in the enum. However, if the enum is already being resolved, this
    // means we have a circular dependency between two enum values. Error
    // about that now.
    if (parent->isResolving()) {
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

void
TypeResolver::visitViewAsExpression(ViewAsExpression* expr)
{
  resolveTypeIfNeeded(expr->te());
}

void
TypeResolver::visitCallNewExpr(CallNewExpr* expr)
{
  resolveTypeIfNeeded(expr->te());
}

void
TypeResolver::visitNewArrayExpr(NewArrayExpr* expr)
{
  resolveTypeIfNeeded(expr->te());
}

void
TypeResolver::visitEnumStatement(EnumStatement* node)
{
  // This should only happen if we resolved the enum before visiting it in
  // the statement list.
  if (node->isResolved())
    return;

  node->setResolving();

  Type* type = node->sym()->type();

  int value = 0;
  for (size_t i = 0; i < node->entries()->size(); i++) {
    EnumConstant* cs = node->entries()->at(i);
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

  node->setResolved();
}

EnumType*
TypeResolver::resolveMethodmapParentType(NameProxy* proxy)
{
  // The parent must be a methodmap.
  TypeSymbol* parentSymbol = proxy->sym()->asType();
  if (!parentSymbol) {
    cc_.report(proxy->loc(), rmsg::bad_methodmap_parent)
      << proxy->name();
    return nullptr;
  }

  // Don't error twice if we get an unresolved type.
  Type* type = resolveNameToType(proxy);
  if (type->isUnresolvable())
    return nullptr;

  if (!type->isEnum() || !type->toEnum()->methodmap()) {
    cc_.report(proxy->loc(), rmsg::bad_methodmap_parent_type)
      << type;
    return nullptr;
  }

  return type->toEnum();
}

// :TODO: test recursive definitions
void
TypeResolver::visitMethodmapDecl(MethodmapDecl* methodmap)
{
  EnumType* type = methodmap->sym()->type()->toEnum();
  if (type->methodmap())
    return;

  // Create a methodmap now so we don't re-enter.
  type->setMethodmap(methodmap);

  // Our parent must be resolved first.
  EnumType* parent = nullptr;
  if (methodmap->parent())
    parent = resolveMethodmapParentType(methodmap->parent());

  // Check that we do not appear in the parent chain.
  for (EnumType* cursor = parent; cursor; cursor = cursor->methodmap()->extends()) {
    if (cursor->methodmap() == methodmap) {
      cc_.report(methodmap->parent()->loc(), rmsg::circular_methodmap)
        << methodmap->name();
      parent = nullptr;
      break;
    }
  }
  methodmap->setExtends(parent);
}

void
TypeResolver::visitFunction(FunctionNode* node)
{
  if (!node->signature()->isResolved()) {
    resolveTypesInSignature(node->signature());
    assignTypeToFunction(node);
  }
}

void
TypeResolver::assignTypeToFunction(FunctionNode* node)
{
  assert(!node->signature_type());
  Type* type = cc_.types()->newFunction(node->signature());
  node->set_signature_type(type);
}

void
TypeResolver::visitFieldDecl(FieldDecl* decl)
{
  Type* type = resolveTypeIfNeeded(decl->te());
  decl->sym()->setType(type);
}

void
TypeResolver::visitPropertyDecl(PropertyDecl* decl)
{
  // It is impossible to use property decl types in constant exprs or
  // type resolvers. I don't anticipate this ever being a thing either.
  if (!decl->sym()->type())
    decl->sym()->setType(resolveTypeIfNeeded(decl->te()));

  if (FunctionNode* getter = decl->getter())
    visitFunction(getter);
  if (FunctionNode* setter = decl->setter())
    visitFunction(setter);
}

void
TypeResolver::visitMethodDecl(MethodDecl* decl)
{
  // It is not possible to refer to methods as part of a constant or type
  // expression - no re-entrancy or upward-resolving needed.
  if (FunctionNode* node = decl->method())
    visitFunction(node);
}

void
TypeResolver::visitFunctionStatement(FunctionStatement* node)
{
  visitFunction(node);
}

void
TypeResolver::visitTypedefDecl(TypedefDecl* node)
{
  // Re-entrancy guard.
  if (node->te().resolved())
    return;

  TypedefType* type = node->sym()->type()->toTypedef();
  const SourceLocation& baseLoc = node->te().spec()->baseLoc();

  // This is our recursion guard.
  Type* actual = resolveType(node->te());
  
  // Check for a recursive type. There might be a better way to do this, but
  // for now this is what we've got: just manually check any compound type
  // that wouldn't be able to handle a self-reference.
  Type* base = actual;
  if (ArrayType* array = base->asArray()) {
    while (array->contained()->isArray())
      array = array->contained()->toArray();
    base = array->contained();
  }

  if (type == base->canonical()) {
    type->resolve(&UnresolvableType);
    cc_.report(baseLoc, rmsg::recursive_type);
    return;
  }

  assignTypeToTypedef(node, type, actual);
}

void
TypeResolver::visitTypesetDecl(TypesetDecl* decl)
{
  if (!decl->isResolved())
    return;

  for (size_t i = 0; i < decl->types()->size(); i++) {
    TypeExpr& te = decl->types()->at(i).te;
    resolveTypeIfNeeded(te);
  }

  if (!verifyTypeset(decl))
    return;

  TypesetType::TypeList* list =
    new (pool_) TypesetType::TypeList(decl->types()->size());
  for (size_t i = 0; i < list->size(); i++) {
    TypesetDecl::Entry& entry = decl->types()->at(i);
    list->at(i) = entry.te.resolved();
  }

  TypesetType* type = decl->sym()->type()->toTypeset();
  type->setTypes(list);

  decl->setResolved();
}

bool
TypeResolver::verifyTypeset(TypesetDecl* decl)
{
  bool ok = true;

  // Verify that types aren't duplicated. This is an O(n^2) algorithm - we
  // assume N will be very small.
  TypesetDecl::Entries* types = decl->types();
  for (size_t i = 0; i < types->size(); i++) {
    TypesetDecl::Entry& entry = types->at(i);
    Type* current = entry.te.resolved();

    // :TODO: handle const int vs int

    for (size_t j = 0; j < i; j++) {
      TypesetDecl::Entry& prevEntry = types->at(j);
      Type* prev = prevEntry.te.resolved();

      if (AreTypesEquivalent(prev, current, Qualifiers::None)) {
        cc_.report(entry.loc, rmsg::typeset_ambiguous_type)
          << current << decl->name()
          << (cc_.note(prevEntry.loc, rmsg::previous_location));
        ok = false;
        break;
      }
    }

    if (!current->isFunction())
      cc_.report(entry.loc, rmsg::typeset_must_only_have_fun);
  }

  return ok;
}

// Returns true if it could be resolved to a constant integer; false
// otherwise. |outp| is unmodified on failure.
bool
TypeResolver::resolveConstantArraySize(ConstantEvaluator::Mode mode,
                                       Expression* expr,
                                       int* outp)
{
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

  const IntValue& iv = value.toInteger();
  if (iv.isNegativeOrZero()) {
    cc_.report(expr->loc(), rmsg::array_size_must_be_positive);
    return false;
  }
  if (!iv.valueFitsInInt32()) {
    cc_.report(expr->loc(), rmsg::array_size_too_large);
    return false;
  }
  if (iv.asInt32() > ArrayType::kMaxSize) {
    cc_.report(expr->loc(), rmsg::array_size_too_large);
    return false;
  }

  *outp = iv.asInt32();
  return true;
}

void
TypeResolver::computeFixedArraySizes(TypeSpecifier* spec,
                                     Type* base,
                                     std::vector<Rank>& ranks,
                                     size_t rank_index,
                                     ArrayLiteral* list)
{
  // :TODO: test when literals are too deeply nested
  if (rank_index >= ranks.size())
    return;

  Rank& rank = ranks[rank_index];
  for (size_t i = 0; i < list->expressions()->size(); i++) {
    Expression* expr = list->expressions()->at(i);
    int size = -1;
    if (ArrayLiteral* lit = expr->asArrayLiteral()) {
      size = lit->arrayLength();
      computeFixedArraySizes(spec, base, ranks, rank_index + 1, lit);
    } else if (StringLiteral* lit = expr->asStringLiteral()) {
      // String literals are only valid in the last rank.
      if (rank_index == spec->rank() - 1)
        size = lit->arrayLength();
      else
        size = -1;
    } else {
      // If we get here, we either have an invalid construction that looks
      // something like:
      //  int a[][] = { 3 };
      //
      // I.e., the user has specified more array dimensions than there are
      // arrays in the literal.
    }

    if (rank.status == RankStatus::Indeterminate ||
        rank.status == RankStatus::Determinate)
    {
      continue;
    }

    if (size == -1) {
      rank.status = RankStatus::Indeterminate;

      cc_.report(expr->loc(), rmsg::incomplete_array_literal);
    } else if (rank.status == RankStatus::Unvisited) {
      rank.status = RankStatus::Computed;
      rank.size = size;
    } else {
      assert(rank.status == RankStatus::Computed);
      if (base->isPrimitive(PrimitiveType::Char)) {
        // Strings are null-terminated, so we pick the maximum size of any
        // entry in the last dimension.
        rank.size = std::max(rank.size, size);
      } else if (rank.size != size) {
        rank.status = RankStatus::Indeterminate;

        cc_.report(expr->loc(), rmsg::array_literal_size_mismatch);
      }
    }
  }
}

std::vector<Rank>
TypeResolver::fixedArrayLiteralDimensions(TypeSpecifier* spec, Type* base, Expression* init)
{
  std::vector<Rank> ranks;
  for (size_t i = 0; i < spec->rank(); i++) {
    Rank rank;
    if (spec->sizeOfRank(i))
      rank.status = RankStatus::Determinate;
    ranks.push_back(rank);
  }

  // Peel off dimensions where the size is already known.
  while (!ranks.empty() && ranks.back().status == RankStatus::Determinate)
    ranks.pop_back();

  if (ranks.empty())
    return ranks;

  if (StringLiteral* str = init->asStringLiteral()) {
    if (spec->rank() != 1) {
      cc_.report(init->loc(), rmsg::incomplete_array_literal);
      return ranks;
    }

    ranks[0].status = RankStatus::Computed;
    ranks[0].size = str->arrayLength();
    return ranks;
  }

  ArrayLiteral* lit = init->asArrayLiteral();
  if (!lit) {
    cc_.report(init->loc(), rmsg::incomplete_array_literal);
    return ranks;
  }

  // Some dimensions were unsized. Compute fixed sizes (if possible) from
  // the initializer. This is a recursive process so we can try to create
  // uniform fixed lengths for each sub-array, as SourcePawn 1 did.
  if (ranks[0].status != RankStatus::Determinate) {
    ranks[0].status = RankStatus::Computed;
    ranks[0].size = lit->arrayLength();
  }
  if (ranks.size() > 1)
    computeFixedArraySizes(spec, base, ranks, 1, lit);

  // If we have no indeterminate arrays, then everything should have been
  // visited. If we do have indeterminate arrays, an error has been reported.
  // This assertion is to make sure we did indeed report an error.
#if !defined(NDEBUG)
  bool has_indeterminate = false;
  bool has_unvisited = false;
  for (size_t i = 0; i < ranks.size(); i++) {
    if (ranks[i].status == RankStatus::Indeterminate)
      has_indeterminate = true;
    else if (ranks[i].status == RankStatus::Unvisited)
      has_unvisited = true;
  }
  if (has_unvisited)
    assert(has_indeterminate);
#endif

  // Return the computed list unadultered - resolveArrayComponentTypes()
  // will correctly merge with the TypeSpecifier.
  return ranks;
} 

void
TypeResolver::resolveTypesInSignature(FunctionSignature* sig)
{
  resolveType(sig->returnType());
  for (size_t i = 0; i < sig->parameters()->size(); i++) {
    VarDecl* param = sig->parameters()->at(i);

    // We should not have variadic arguments on anything but the last type. It
    // is hard to assert this anywhere else in TypeResolver unfortunately.
#if !defined(NDEBUG)
    if (TypeSpecifier* spec = param->te().spec()) {
      if (i != sig->parameters()->size() - 1)
        assert(!spec->isVariadic());
    }
#endif

    param->accept(this);
  }
}

// The main entrypoint for resolving ConstantSymbols.
void
TypeResolver::resolveConstant(ConstantSymbol* sym)
{
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

Type*
TypeResolver::resolveType(TypeExpr& te, TypeSpecHelper* helper)
{
  if (te.resolved())
    return te.resolved();

  TypeSpecifier* spec = te.spec();
  if (spec->isResolving()) {
    cc_.report(spec->baseLoc(), rmsg::recursive_type);

    // We don't want to report this twice, so mark it as resolved.
    te.setResolved(&UnresolvableType);
    return te.resolved();
  }

  spec->setResolving();

  Type* baseType = resolveBaseType(spec);
  if (!baseType) {
    // Return a placeholder so we don't have to check null everywhere.
    te.setResolved(&UnresolvableType);
    return te.resolved();
  }

  Type* type = baseType;

  // See the big note in applyConstQualifier. For array types, the const
  // is applied to the inner type. The same is true for reference types.
  if (spec->isConst())
    type = applyConstQualifier(spec, type);

  // Create types for each rank of an array.
  if (spec->rank())
    type = resolveArrayComponentTypes(spec, type, helper);

  // If we already had a type here, we must have seen a recursive type. It's
  // okay to rewrite it since we already reported an error somewhere.
  assert(!te.resolved() || te.resolved()->isUnresolvable());

  te.setResolved(type);
  return type;
}

Type*
TypeResolver::resolveBaseType(TypeSpecifier* spec)
{
  switch (spec->resolver()) {
    case TOK_LABEL:
    case TOK_NAME:
      return resolveNameToType(spec->proxy());

    case TOK_DEFINED:
      return spec->getResolvedBase();

    case TOK_FUNCTION:
    {
      FunctionSignature* sig = spec->signature();
      if (!sig->isResolved())
        resolveTypesInSignature(sig);
      return FunctionType::New(sig);
    }

    default:
      // Other cases should have been handled during name resolution.
      assert(false);
      return nullptr;
  }
}

bool
TypeResolver::checkArrayInnerType(TypeSpecifier* spec, Type* type)
{
  if (type->isVoid() || type->isStruct()) {
    cc_.report(spec->arrayLoc(), rmsg::invalid_array_base) << type;
    return false;
  }
  return true;
}

Type*
TypeResolver::applyConstQualifier(TypeSpecifier* spec, Type* type)
{
  // :TODO: spec ref
  //
  // Originally, we considered "const" to be transitive. This is no longer the
  // case. Our Semantic Analysis understands a more flexible and maintainable
  // view, that is backward compatible with SP1 and more forward-proof.
  //
  // "const" is a storage specifier, like in C/C++. It specifies that an
  // l-value is not mutable. It has almost no meaning in other contexts. For
  // example, a return type can be "const int" through a typedef, but the const
  // has no purpose since return values cannot be l-values in SourcePawn.
  //
  // Consider the following cases:
  //
  //    const int[] x = new int[500];
  //    const int y[500];
  //
  // Reassigning |x| is currently not possible in SourcePawn, there are no
  // semantics for copying or acquiring pointers to dynamic arrays. In fact,
  // const does not work at all for this case in the SP1 compiler. However,
  // assigning to the contents of an array declared as const is illegal,
  // no matter its type or whether it is fixed-length.
  //
  // Reassigning |y| is the same as assigning to each element, so a const
  // anywhere on |y| is effectively transitive.
  //
  // We can summarize these two cases across all types: |x| is a pointer type,
  // and |y| is a value type. |y|'s case is quite straightforward, but |x|
  // has no existing semantics. While (for arrays), we must prevent internal
  // modifications, it is unknown whether "const" should also refer to the
  // pointer value. For structs, and objects, it is even less clear.
  //
  // For now we simply preserve SP1 semantics, and we do this by making the
  // *internal* type const, not the external type, similar to C++.
  //
  // I.e., in Pawn, "const int[] x" is really: an array of constant integers.
  // In the future, we can make HandleVarDecl wrap a further const if we want
  // it to, making the actual pointer immutable.
  assert(spec->isConst());

  if (!TypeSupportsConstKeyword(type)) {
    cc_.report(spec->constLoc(), rmsg::type_cannot_be_const)
      << type;
    return type;
  }

  return cc_.types()->newQualified(type, Qualifiers::Const);
}

Type*
TypeResolver::resolveNameToType(NameProxy* proxy)
{
  assert(proxy->sym());

  TypeSymbol* sym = proxy->sym()->asType();
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

Type*
TypeResolver::resolveArrayComponentTypes(TypeSpecifier* spec, Type* type, TypeSpecHelper* helper)
{
  checkArrayInnerType(spec, type);

  // For each rank, try to compute a constant size if one was given. There are
  // two scenarios for how array dimensions are computed, when the type is
  // for a variable declaration:
  //
  // 1. "old-style": pre-transitional syntax.
  //   a. If any rank is unspecified, there must be an initializer to infer
  //      dimensions from, and all specified dimensions must be constant.
  //   b. If all ranks are specified, any non-constant rank will generate
  //      a fully dynamic array.
  // 2. "new-style": transitional syntax.
  //   a. Ranks cuddling the type may not have any size specified. The size
  //      is determined at runtime via an initializer (such as "new").
  //   b. Ranks cuddling the variable name ("post dims") may only have
  //      constant sizes. Any rank that is not specified must be inferred
  //      by a literal.
  //
  // For anything other than block local variables, non-constant expressions
  // are not allowed as dimension sizes, and sizes cannot be inferred.
  VariableSymbol* sym = nullptr;
  Expression* initializer = nullptr;
  ConstantEvaluator::Mode mode = ConstantEvaluator::Required;
  if (helper) {
    if (VarDecl* decl = helper->decl()) {
      sym = decl->sym();
      if (!sym->isArgument())
        initializer = helper->initializer();
      if (spec->isOldDecl() && sym->scope()->kind() == Scope::Block)
        mode = ConstantEvaluator::Speculative;
    }
  }

  // For each rank that has a size expression, compute its constant value if
  // any, and track whether any of the values were not constant. Note that
  // if a rank is missing a size expression, we might still try to infer it
  // later, so all_constant stays true even if no ranks have sizes.
  std::vector<int> given_rank_sizes;
  bool all_constant = true;
  for (size_t i = 0; i < spec->rank(); i++) {
    int rank_size = ArrayType::kUnsized;
    Expression* expr = spec->sizeOfRank(i);
    if (expr && !resolveConstantArraySize(mode, expr, &rank_size))
      all_constant = false;
    given_rank_sizes.push_back(rank_size);
  }

  // Try to also infer the sizes of ranks from an initializer. This is used
  // only when the entire array is fixed-size, and only when the rank size
  // was not specified.
  std::vector<Rank> inferred_sizes;
  if (initializer && all_constant)
    inferred_sizes = fixedArrayLiteralDimensions(spec, type, initializer);

  bool reported_size_error = false;

  size_t rank_index = spec->rank() - 1;
  do {
    int rank_size = ArrayType::kUnsized;
    if (spec->sizeOfRank(rank_index)) {
      // Should either be old-style, or fixed-array style.
      assert(spec->isOldDecl() || spec->hasPostDims());

      // If all ranks were constant, we can use the constant size for this
      // rank. Otherwise, we must treat every slot as having an unknown size.
      //
      // Any errors at this point were reported by ConstantEvaluator, so we
      // don't report any inconsistencies here.
      if (all_constant)
        rank_size = given_rank_sizes[rank_index];
    } else if (rank_index < inferred_sizes.size()) {
      // Should either be an old-style or fixed-array style local or global
      // variable.
      assert(spec->isOldDecl() || spec->hasPostDims());
      assert(!sym->isArgument());
      assert(all_constant);

      const Rank& r = inferred_sizes[rank_index];
      assert(r.status != RankStatus::Unvisited);
      assert(r.status != RankStatus::Determinate);

      // If the status is Indeterminate, then we already threw an error earlier,
      // so we just keep going to gather any more errors.
      if (r.status == RankStatus::Computed)
        rank_size = r.size;
    } else if (all_constant &&
               (sym && sym->isArgument()) &&
               (spec->isOldDecl() || spec->hasPostDims()))
    {
      // This is a local variable declaration that is missing a size or the
      // size could not be inferred. Currently, we do not allow 0-length or
      // null arrays to be declared.
      //
      // Note that this rule doesn't apply to new-style array declarations.
      // If the user forgot an initializer, they'll get an error in the
      // SemA pass. Here we're only trying to catch variable types that are
      // undeclarable.
      if (!reported_size_error) {
        cc_.report(spec->arrayLoc(), rmsg::new_array_missing_dimension) <<
          rank_index;
        reported_size_error = true;
      }
    }

    if (!reported_size_error && rank_size > ArrayType::kMaxSize) {
      cc_.report(spec->arrayLoc(), rmsg::array_size_too_large);
      reported_size_error = true;
    }

    // :TODO: constify
    type = cc_.types()->newArray(type, rank_size);
  } while (rank_index--);
  return type;
}

bool
TypeResolver::resolveEnumConstantValue(EnumConstant* cs, int* outp)
{
  AutoPush<EnumConstant*> track(enum_constant_stack_, cs);

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

// Like applyConstQualifier, this must not return null (its callers do not
// null check). Instead, return the input type on error.
Type*
TypeResolver::applyByRef(TypeSpecifier* spec, Type* type, TypeSpecHelper* helper)
{
  if (!type->canBeUsedAsRefType()) {
    cc_.report(spec->byRefLoc(), rmsg::type_cannot_be_ref) << type;
    return type;
  }

  VarDecl* decl = helper ? helper->decl() : nullptr;
  if (decl)
    assert(decl->sym()->isArgument());

  return cc_.types()->newReference(type);
}

Type*
TypeResolver::applyVariadic(TypeSpecifier* spec, Type* type, TypeSpecHelper* helper)
{
  assert(!type->isVariadic());
  assert(spec->isVariadic());

  if (type->isReference()) {
    SourceLocation pos = spec->isByRef() ? spec->byRefLoc() : spec->baseLoc();
    cc_.report(pos, rmsg::vararg_cannot_be_byref);
    return type;
  }
  if (type->isVoid()) {
    cc_.report(spec->baseLoc(), rmsg::illegal_vararg_type) << type;
    return type;
  }

  return cc_.types()->newVariadic(type);
}

static inline bool
IsAllowableStructDecl(VariableSymbol* sym, Type* type)
{
  return type->isStruct() &&
         sym->scope()->kind() == Scope::Global &&
         sym->node()->toVarDecl()->classifier() == TOK_PUBLIC;
}

bool
TypeResolver::assignTypeToSymbol(VariableSymbol* sym, Type* type)
{
  if (!type)
    return false;

  assert(!type->isReference() || sym->isArgument());

  // :TODO: why is this here? move it into sema?
  if (!type->isVariadic() && !type->isStorableType()) {
    // We make a very specific exception for public structs, which are barely supported.
    if (!IsAllowableStructDecl(sym, type)) {
      cc_.report(sym->node()->loc(), rmsg::cannot_use_type_in_decl) << type;
      return false;
    }
  }

  sym->setType(type);
  return true;
}

void
TypeResolver::assignTypeToTypedef(TypedefDecl* decl, TypedefType* def, Type* actual)
{
  if (!actual->isFunction()) {
    cc_.report(decl->loc(), rmsg::typedef_must_be_fun);
    return;
  }

  def->resolve(actual);
}

} // namespace sp
