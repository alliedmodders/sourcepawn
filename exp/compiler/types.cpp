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
#include "types.h"
#include "compile-context.h"
#include "boxed-value.h"
#include "parser/ast.h"
#include "source-manager.h"

using namespace ke;
using namespace sp;
using namespace sp::ast;

Type sp::UnresolvableType(Type::Kind::Unresolvable);

Type*
Type::NewVoid()
{
  return new (POOL()) Type(Kind::Void);
}

Type*
Type::NewImplicitVoid()
{
  return new (POOL()) Type(Kind::ImplicitVoid);
}

Type*
Type::NewNullType()
{
  return new (POOL()) Type(Kind::NullType);
}

Type*
Type::NewUnchecked()
{
  Type* type = new (POOL()) Type(Kind::Unchecked);
  return type;
}

Type*
Type::NewMetaFunction()
{
  return new (POOL()) Type(Kind::MetaFunction);
}

Type*
Type::NewOverloadedFunction()
{
  return new (POOL()) Type(Kind::OverloadedFunction);
}

Type*
Type::NewPrimitive(PrimitiveType prim)
{
  Type* type = new (POOL()) Type(Kind::Primitive);
  type->primitive_ = prim;
  return type;
}

Type*
Type::NewQualified(Type* type, Qualifiers qualifiers)
{
  assert(!type->isQualified());
  Type* qual = new (POOL()) Type(Kind::Qualifier, type);
  qual->qualifiers_ = qualifiers;
  return qual;
}

ArrayType*
ArrayType::New(Type* contained, int elements)
{
  ArrayType* type = new (POOL()) ArrayType(Kind::Array);
  type->contained_ = contained;
  type->elements_ = elements;
  type->nlevels_ = contained->isArray()
                   ? contained->toArray()->nlevels_ + 1
                   : 1;
  return type;
}

EnumType*
EnumType::New(Atom* name)
{
  EnumType* type = new (POOL()) EnumType();
  type->kind_ = Kind::Enum;
  type->name_ = name;
  return type;
}

TypedefType*
TypedefType::New(Atom* name)
{
  TypedefType* tdef = new (POOL()) TypedefType(name);
  tdef->actual_ = nullptr;
  return tdef;
}

void
TypedefType::resolve(Type* actual)
{
  canonical_ = actual;
  actual_ = actual;
}

bool
Type::Compare(Type* left, Type* right)
{
  if (left == right)
    return true;

  if (left->kind_ != right->kind_)
    return false;

  switch (left->kind_) {
    case Kind::Primitive:
      return left->primitive() == right->primitive();

    case Kind::Array:
    {
      ArrayType* aleft = left->toArray();
      ArrayType* aright = right->toArray();
      return aleft->equalTo(aright);
    }

    case Kind::Function:
      // :TODO:
      return false;

    case Kind::Enum:
    case Kind::Typedef:
      return false;

    case Kind::Void:
      return true;

    default:
      assert(false);
      return false;
  }
}

FunctionType*
FunctionType::New(FunctionSignature* sig)
{
  return new (POOL()) FunctionType(sig);
}

Type*
FunctionType::returnType() const
{
  return signature_->returnType().resolved();
}

TypesetType*
TypesetType::New(Atom* name)
{
  return new (POOL()) TypesetType(name);
}

StructType::StructType(ast::RecordDecl* decl)
 : RecordType(Kind::Struct, decl)
{
}

Atom*
RecordType::name() const
{
  return decl_->name();
}

StructType*
StructType::New(ast::RecordDecl* decl)
{
  return new (POOL()) StructType(decl);
}

ReferenceType*
ReferenceType::New(Type* inner)
{
  assert(inner->canBeUsedAsRefType());
  return new (POOL()) ReferenceType(inner);
}

VariadicType*
VariadicType::New(Type* inner)
{
  assert(!inner->isVariadic());
  assert(!inner->isReference());
  assert(!inner->isVoid());
  return new (POOL()) VariadicType(inner);
}

const char*
sp::GetPrimitiveName(PrimitiveType type)
{
  switch (type) {
    case PrimitiveType::Bool:
      return "bool";
    case PrimitiveType::Char:
      return "char";
    case PrimitiveType::Float:
      return "float";
#if 0
    case PrimitiveType::Double:
      return "double";
    case PrimitiveType::Int8:
      return "int8";
    case PrimitiveType::Uint8:
      return "uint8";
    case PrimitiveType::Int16:
      return "int16";
    case PrimitiveType::Uint16:
      return "uint16";
#endif
    case PrimitiveType::Int32:
      return "int";
#if 0
    case PrimitiveType::Uint32:
      return "uint";
    case PrimitiveType::Int64:
      return "int64";
    case PrimitiveType::Uint64:
      return "uint64";
    case PrimitiveType::NativeInt:
      return "intn";
    case PrimitiveType::NativeUint:
      return "uintn";
#endif
    default:
      assert(false);
      return "unknown";
  }
}

static const char*
GetBaseTypeName(Type* type)
{
  if (type->isUnresolvable())
    return "<unresolved>";
  if (type->isMetaFunction())
    return "function";
  if (type->isUnchecked())
    return "any";
  if (type->isVoid() || type->isImplicitVoid())
    return "void";
  if (type->isTypeset())
    return type->toTypeset()->name()->chars();
  if (type->isEnum())
    return type->toEnum()->name()->chars();
  if (type->isRecord())
    return type->toRecord()->name()->chars();
  if (TypedefType* tdef = type->asTypedef())
    return tdef->name()->chars();
  if (ReferenceType* rdef = type->asReference())
    return GetBaseTypeName(rdef->inner());
  return GetPrimitiveName(type->primitive());
}

static std::string BuildTypeFromSpecifier(const TypeSpecifier* spec, Atom* name, TypeDiagFlags flags);
static std::string BuildTypeFromSignature(const FunctionSignature* sig, TypeDiagFlags flags);

// When building inner typenames, only include these flags.
static const TypeDiagFlags kDiagFlagsInnerMask =
  TypeDiagFlags::Names;

static std::string
BuildTypeFromTypeExpr(const TypeExpr& te, Atom* name, TypeDiagFlags flags)
{
  if (te.spec())
    return BuildTypeFromSpecifier(te.spec(), name, flags);
  return BuildTypeName(te.resolved(), name, flags);
}

static std::string
BuildTypeFromSignature(const FunctionSignature* sig, TypeDiagFlags flags)
{
  AutoString base = "function ";
  base = base + BuildTypeFromTypeExpr(sig->returnType(), nullptr, flags & kDiagFlagsInnerMask);
  base = base + "(";

  for (size_t i = 0; i < sig->parameters()->size(); i++) {
    TypeDiagFlags varFlags = flags & kDiagFlagsInnerMask;
    Atom* name = !!(flags & TypeDiagFlags::Names)
                 ? sig->parameters()->at(i)->name()
                 : nullptr;
    VarDecl* decl = sig->parameters()->at(i);
    // If the type was not resolved, use the te instead.
    if ((decl->type() && decl->type()->isReference()) ||
        (decl->te().spec() && decl->te().spec()->isByRef()))
    {
      varFlags |= TypeDiagFlags::IsByRef;
    }
    base = base + BuildTypeFromTypeExpr(sig->parameters()->at(i)->te(), name, varFlags);
    if (i != sig->parameters()->size() - 1)
      base = base + ", ";
  }
  base = base + ")";
  return std::string(base.ptr());
}

static std::string
BuildTypeFromSpecifier(const TypeSpecifier* spec, Atom* name, TypeDiagFlags flags)
{
  AutoString base;
  if (spec->isConst() || !!(flags & TypeDiagFlags::IsConst))
    base = "const ";

  switch (spec->resolver()) {
    case TOK_LABEL:
    {
      // HACK: we should move these atoms into the context.
      const char* chars = spec->proxy()->name()->chars();
      if (strcmp(chars, "Float") == 0)
        base = base + "float";
      else if (strcmp(chars, "String") == 0)
        base = base + "char";
      else if (strcmp(chars, "_") == 0)
        base = base + "int";
      else
        base = base + chars;
      break;
    }
    case TOK_NAME:
      base = base + spec->proxy()->name()->chars();
      break;
    case TOK_IMPLICIT_INT:
      base = base + "int";
      break;
    case TOK_FUNCTION:
      base = base + BuildTypeFromSignature(spec->signature(), flags & kDiagFlagsInnerMask);
      break;
    case TOK_DEFINED:
      base = base + BuildTypeName(spec->getResolvedBase(), nullptr, flags & kDiagFlagsInnerMask);
      break;
    default:
      base = base + TokenNames[spec->resolver()];
      break;
  }

  // We need type analysis to determine the true type, so just make a semi-
  // intelligent guess based on whether or not any rank has a sized dimension.
  bool postDims = (spec->isNewDecl() && spec->hasPostDims()) ||
                  (spec->isOldDecl() && spec->dims());

  if (name && postDims)
    base = base + " " + name->chars();

  for (size_t i = 0; i < spec->rank(); i++) {
    if (Expression* expr = spec->sizeOfRank(i)) {
      if (IntegerLiteral* lit = expr->asIntegerLiteral()) {
        char value[24];
        SafeSprintf(value, sizeof(value), "%d", (int)lit->value());
        base = base + "[" + value + "]";
        continue;
      }
    }

    // Hack. We can do better if it becomes necessary, these types are only
    // for diagnostics.
    base = base + "[]";
  }
  if (name && !postDims) {
    base = base + " ";
    if (spec->isByRef())
      base = base + "&";
    base = base + name->chars();
  } else {
    if (spec->isByRef())
      base = base + "&";
  }

  if (spec->isVariadic())
    base = base + " ...";

  return std::string(base.ptr());
}

std::string
sp::BuildTypeName(const TypeSpecifier* spec, Atom* name, TypeDiagFlags flags)
{
  return BuildTypeFromSpecifier(spec, name, flags);
}

std::string
sp::BuildTypeName(Type* aType, Atom* name, TypeDiagFlags flags)
{
  bool variadic = false;
  if (aType->isVariadic()) {
    aType = aType->toVariadic()->inner();
    variadic = true;
  }

  if (ArrayType* type = aType->asArray()) {
    std::vector<ArrayType*> stack;

    Type* cursor = type;
    Type* innermost = nullptr;
    for (;;) {
      if (!cursor->isArray()) {
        innermost = cursor;
        break;
      }
      stack.push_back(cursor->toArray());
      cursor = cursor->toArray()->contained();
    }

    AutoString builder;
    if (innermost->isConst() || !!(flags & TypeDiagFlags::IsConst))
      builder = "const ";

    builder = builder + BuildTypeName(innermost->unqualified(), nullptr, flags & kDiagFlagsInnerMask);

    bool hasFixedLengths = false;
    AutoString brackets;
    for (size_t i = 0; i < stack.size(); i++) {
      if (!stack[i]->hasFixedLength()) {
        brackets = brackets + "[]";
        continue;
      }

      hasFixedLengths = true;
      brackets = brackets + "[" + AutoString(stack[i]->fixedLength()) + "]";
    }

    if (name) {
      if (hasFixedLengths)
        builder = builder + " " + name->chars() + brackets;
      else
        builder = builder + brackets + " " + name->chars();
    } else {
      builder = builder + brackets;
    }
    if (variadic)
      builder = builder + "...";
    return std::string(builder.ptr());
  }

  AutoString builder;

  if (aType->isConst())
    builder = "const ";

  if (FunctionType* type = aType->asFunction())
    builder = builder + BuildTypeFromSignature(type->signature(), flags & kDiagFlagsInnerMask);
  else
    builder = builder + GetBaseTypeName(aType);

  if (!!(flags & TypeDiagFlags::IsByRef) || aType->isReference())
    builder = builder + "&";

  if (name)
    builder = builder + " " + name->chars();

  if (variadic)
    builder = builder + "...";

  return std::string(builder.ptr());
}

std::string
sp::BuildTypeName(const TypeExpr& te, Atom* name, TypeDiagFlags flags)
{
  if (te.spec())
    return BuildTypeName(te.spec(), name, flags);
  return BuildTypeName(te.resolved(), name, flags);
}

BoxedValue
sp::DefaultValueForPlainType(Type* type)
{
  if (type->isUnresolvable())
    return BoxedValue(IntValue::FromInt32(0));
  if (type->isEnum())
    return BoxedValue(IntValue::FromInt32(0));
  switch (type->primitive()) {
    case PrimitiveType::Bool:
      return BoxedValue(false);
    case PrimitiveType::Char:
      return BoxedValue(IntValue::FromInt8(0));
    case PrimitiveType::Int32:
      return BoxedValue(IntValue::FromInt32(0));
    case PrimitiveType::Float:
      return BoxedValue(FloatValue::FromFloat(0));
  }
  assert(false);
  return BoxedValue(IntValue::FromInt32(0));
}

int32_t
sp::ComputeSizeOfType(ReportingContext& cc, Type* aType, size_t level)
{
  if (aType->isUnresolvedTypedef()) {
    cc.report(rmsg::recursive_type);
    return 0;
  }
  if (!aType->isArray()) {
    cc.report(rmsg::sizeof_needs_array);
    return 0;
  }

  ArrayType* type = aType->toArray();
  for (size_t i = 1; i <= level; i++) {
    if (!type->contained()->isArray()) {
      cc.report(rmsg::sizeof_invalid_rank);
      return 0;
    }
    type = type->contained()->toArray();
  }

  if (!type->hasFixedLength()) {
    cc.report(rmsg::sizeof_indeterminate);
    return 0;
  }

  return type->fixedLength();
}

bool
sp::AreFunctionTypesEqual(FunctionType* a, FunctionType* b)
{
  FunctionSignature* af = a->signature();
  FunctionSignature* bf = b->signature();
  if (!AreTypesEquivalent(af->returnType().resolved(),
                          bf->returnType().resolved(),
                          Qualifiers::None))
  {
    return false;
  }

  ParameterList* ap = af->parameters();
  ParameterList* bp = bf->parameters();
  if (ap->size() != bp->size())
    return false;

  for (size_t i = 0; i < ap->size(); i++) {
    VarDecl* arga = ap->at(i);
    VarDecl* argb = bp->at(i);
    if (!AreTypesEquivalent(arga->te().resolved(),
                            argb->te().resolved(),
                            Qualifiers::None))
    {
      return false;
    }
  }
  return true;
}

// Since const is transitive, we require it to be threaded through type
// equivalence tests.
bool
sp::AreTypesEquivalent(Type* a, Type* b, Qualifiers context)
{
  Qualifiers qa = (a->qualifiers() | context);
  Qualifiers qb = (b->qualifiers() | context);
  if (qa != qb)
    return false;

  a = a->canonical();
  b = b->canonical();
  if (a == b)
    return true;

  switch (a->canonicalKind()) {
    case Type::Kind::Primitive:
      // Either |b| is not primitive, or they should not have the same
      // primitive type since each type is a singleton.
      assert(!b->isPrimitive() || a->primitive() != b->primitive());
      return false;
    case Type::Kind::Function:
    {
      if (!b->isFunction())
        return false;

      // const is not transitive through function signatures.
      return AreFunctionTypesEqual(a->toFunction(), b->toFunction());
    }
    case Type::Kind::Array:
    {
      if (!b->isArray())
        return false;

      ArrayType* aa = a->toArray();
      ArrayType* ba = b->toArray();
      while (true) {
        // Both arrays must be either dynamic or have the same fixed size.
        if (aa->hasFixedLength() != ba->hasFixedLength())
          return false;
        if (aa->hasFixedLength() && aa->fixedLength() != ba->fixedLength())
          return false;

        // Both contained types must be the same type.
        Type* innerA = aa->contained();
        Type* innerB = ba->contained();
        if (innerA->isArray() != innerB->isArray())
          return false;
        if (!innerA->isArray()) {
          // const is transitive through arrays.
          if (!AreTypesEquivalent(innerA, innerB, context))
            return false;

          // If neither contained type is an array, we're done.
          break;
        }

        // Re-check qualifiers.
        Qualifiers qa = (innerA->qualifiers() | context);
        Qualifiers qb = (innerB->qualifiers() | context);
        if (qa != qb)
          return false;
      }
      return true;
    }
    // These types have unique instances.
    case Type::Kind::Void:
    case Type::Kind::Unchecked:
    case Type::Kind::MetaFunction:
    // These types are named and must have the same identity.
    case Type::Kind::Struct:
    case Type::Kind::Typeset:
    case Type::Kind::Enum:
      // Handled by a == b check earlier.
      return false;
    default:
      // Should not get Unresolvable, Typedef, or Qualifier here.
      assert(false);
      return false;
  }
}
