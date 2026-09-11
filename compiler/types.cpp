/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2024-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#include <ctype.h>

#include <utility>

#include <amtl/am-string.h>
#include "array-helpers.h"
#include "compile-context.h"
#include "parse-node.h"
#include "sc.h"
#include "types.h"

namespace sp {
namespace cc {

using namespace ke;

const char* Type::prettyName() {
  if (kind_ == TypeKind::Function || kind_ == TypeKind::FunctionSignature)
    return kindName();
  if (kind_ == TypeKind::Array && !name_) {
      std::string suffix;
      auto iter = to<ArrayType>();
      for (;;) {
          if (iter->size())
              suffix += ke::StringPrintf("[%d]", iter->size());
          else
              suffix += "[]";
          if (!iter->inner()->isArray())
              break;
          iter = iter->inner()->to<ArrayType>();
      }
      suffix = iter->inner()->prettyName() + suffix;
      name_ = CompileContext::get().atom(suffix);
  }
  return declName()->chars();
}

const char*
Type::kindName() const
{
  switch (kind_) {
    case TypeKind::EnumStruct:
      return "enum struct";
    case TypeKind::Object:
      return "class";
    case TypeKind::Pstruct:
      return "struct";
    case TypeKind::Methodmap:
      return "methodmap";
    case TypeKind::Enum:
      return "enum";
    case TypeKind::Function:
      if (funcenum_ptr_) {
        if (funcenum_ptr_->entries.size() > 1)
          return "typeset";
        if (ke::StartsWith(name_->chars(), "::"))
          return "function";
        return "function";
      }
      return "function";
    case TypeKind::FunctionSignature:
      return "function";
    default:
      return "type";
  }
}

bool Type::isCharArray() const {
    return isArray() && inner()->isChar();
}

bool Type::isLegacyFunction() const {
    if (auto ft = as<FunctionType>())
        return ft->conv() == FunctionType::Convention::Legacy;
    if (kind_ == TypeKind::Function)
        return true;
    return false;
}

bool Type::isNonHeapNullable() const {
    auto map = asMethodmap();
    return map && map->nullable();
}

bool Type::isFlatArray() const {
    if (auto array = as<ArrayType>())
        return array->is_flat();
    return false;
}

bool Type::isFixedArray() const {
    if (auto array = as<ArrayType>())
        return array->is_fixed();
    return false;
}

bool Type::isNonFlatArray() const {
    return isArray() && !isFlatArray();
}

bool Type::isNullable() const {
    if (isClass())
        return true;
    if (auto array = as<ArrayType>())
        return !array->is_flat();
    return isNull();
}

bool Type::isCompositeValue() const {
    return isEnumStruct() || isFlatArray();
}

bool Type::isPassByRef() const {
    return isReference() || isArray() || isWideType() || isCompositeValue();
}

bool Type::isAddressType() const {
    return isReference() || (isArray() && !isCompositeValue());
}

bool Type::isHeapItem() {
    if (isClass())
        return true;
    if (auto at = as<ArrayType>())
        return !at->is_flat();
    if (auto ft = as<FunctionType>())
        return ft->conv() != FunctionType::Legacy;
    return false;
}

Decl* Type::decl() const {
    switch (kind_) {
        case TypeKind::Object:
            return class_ptr_;
        case TypeKind::Enum:
            return enum_ptr_;
        case TypeKind::EnumStruct:
            return enumstruct_ptr_;
        case TypeKind::Methodmap:
            return methodmap_ptr_;
        case TypeKind::Pstruct:
            return pstruct_ptr_;
        default:
            return nullptr;
    }
}

ArrayType::ArrayType(Type* inner, int size, bool is_flat)
  : Type(nullptr, TypeKind::Array)
{
    inner_type_ = inner;
    size_ = size;
    is_flat_ = is_flat;
    if (auto child = inner->as<ArrayType>())
        rank_ = child->rank() + 1;
    else
        rank_ = 0;
    allowed_in_native_call_ = inner->isAllowedInNativeCall();
}

bool FunctionType::needs_hidden_arg() const {
    return return_type_->isFlatArray() ||
           return_type_->isEnumStruct() ||
           return_type_->isWideType();
}

TypeManager::TypeManager(CompileContext& cc)
  : cc_(cc)
{
    array_cache_.init(256);
    function_cache_.init(512);
}

Type* TypeManager::findBuiltin(Atom* atom) {
    auto iter = builtins_.find(atom);
    if (iter == builtins_.end())
        return nullptr;
    return iter->second;
}

Type* TypeManager::Get(int index) {
    return by_index_[index];
}

Type* TypeManager::defineBuiltin(const char* name, BuiltinType type) {
    auto name_atom = cc_.atom(name);
    Type* ptr = new Type(name_atom, TypeKind::Builtin);
    ptr->setBuiltinType(type);

    uint32_t index = (uint32_t)type;
    if (index >= builtin_types_.size())
        builtin_types_.resize(index + 1);
    builtin_types_[index] = ptr;

    [[maybe_unused]] auto result = builtins_.emplace(name_atom, ptr);
    assert(result.second);
    return ptr;
}

ArrayType* TypeManager::defineArray(Type* element_type, int dim) {
    assert(!element_type->isFlatArray());
    auto lookup = ArrayCachePolicy::Lookup{element_type, dim, false};
    auto p = array_cache_.findForAdd(lookup);
    if (!p.found()) {
        auto at = new ArrayType(element_type, dim, false);
        array_cache_.add(p, at);
    }
    return (*p)->to<ArrayType>();
}

ArrayType* TypeManager::defineArray(Type* element_type, const PoolArray<int>& dim_vec) {
    return defineArray(element_type, dim_vec.buffer(), (int)dim_vec.size());
}

ArrayType* TypeManager::defineArray(Type* element_type, const int* dim_vec, int numdim) {
    assert(!element_type->isFlatArray());
    assert(numdim >= 1);

    size_t depth = numdim - 1;
    Type* iter = element_type;
    for (;;) {
        auto lookup = ArrayCachePolicy::Lookup{iter, dim_vec[depth], false};
        auto p = array_cache_.findForAdd(lookup);
        if (!p.found()) {
            auto at = new ArrayType(iter, dim_vec[depth], false);
            array_cache_.add(p, at);
        }
        iter = *p;

        if (!depth)
            break;
        depth--;
    }

    return iter->to<ArrayType>();
}

ArrayType* TypeManager::defineFlatArray(Type* element_type, int dim) {
    assert(!element_type->isArray());
    auto lookup = ArrayCachePolicy::Lookup{element_type, dim, true};
    auto p = array_cache_.findForAdd(lookup);
    if (!p.found()) {
        auto at = new ArrayType(element_type, dim, true);
        array_cache_.add(p, at);
    }
    return (*p)->to<ArrayType>();
}

ArrayType* TypeManager::redefineArray(Type* element_type, ArrayType* old_type) {
    std::vector<int> dim_vec;
    for (auto iter = old_type; iter; iter = iter->inner()->as<ArrayType>()) {
        dim_vec.emplace_back(iter->size());
    }
    if (dim_vec.size() == 1) {
        if (old_type->is_flat())
            return defineFlatArray(element_type, dim_vec[0]);
        return defineArray(element_type, dim_vec[0]);
    }
    return defineArray(element_type, dim_vec.data(), (int)dim_vec.size());
}

void TypeManager::init() {
    type_int_ = defineBuiltin("int", BuiltinType::Int);
    builtins_.emplace(cc_.atom("_"), type_int_);

    type_bool_ = defineBuiltin("bool", BuiltinType::Bool);
    type_any_ = defineBuiltin("any", BuiltinType::Any);

    type_float_ = defineBuiltin("float", BuiltinType::Float);
    builtins_.emplace(cc_.atom("Float"), type_float_);
    type_double_ = defineBuiltin("double", BuiltinType::Double);

    type_void_ = defineBuiltin("void", BuiltinType::Void);
    type_null_ = defineBuiltin("null_t", BuiltinType::Null);

    type_string_ = defineBuiltin("char", BuiltinType::Char);
    builtins_.emplace(cc_.atom("String"), type_string_);

    type_function_ = defineFunction(cc_.atom("Function"), nullptr);
    builtins_.emplace(type_function_->declName(), type_function_);
    type_object_ = defineObject("object");
    builtins_.emplace(type_object_->declName(), type_object_);

    type_int64_ = defineBuiltin("int64", BuiltinType::Int64);
    type_intptr_ = defineBuiltin("intptr", BuiltinType::IntPtr);
    type_int16_ = defineBuiltin("int16", BuiltinType::Int16);
    type_int8_ = defineBuiltin("int8", BuiltinType::Int8);
}

Type* TypeManager::defineFunction(Atom* name, funcenum_t* fe) {
    Type* type = new Type(name, TypeKind::Function);
    type->setFunction(fe);
    return type;
}

Type* TypeManager::defineObject(const char* name) {
    Type* type = new Type(cc_.atom(name), TypeKind::Object);
    type->setObject();
    return type;
}

Type* TypeManager::defineMethodmap(Atom* name, MethodmapDecl* map) {
    Type* type = new Type(name, TypeKind::Methodmap);
    type->setMethodmap(map);
    return type;
}

Type* TypeManager::defineEnumTag(const char* name, EnumDecl* decl) {
    auto type = new Type(cc_.atom(name), TypeKind::Enum);
    type->setEnum(decl);
    return type;
}

Type* TypeManager::defineEnumStruct(Atom* name, EnumStructDecl* decl) {
    Type* type = new Type(name, TypeKind::EnumStruct);
    type->setEnumStruct(decl);
    return type;
}

Type* TypeManager::defineClass(Atom* name, ClassDecl* decl) {
    Type* type = new Type(name, TypeKind::Object);
    type->setClass(decl);
    return type;
}

Type* TypeManager::defineTag(Atom* name) {
    return new Type(name, TypeKind::Enum);
}

Type*
TypeManager::definePstruct(PstructDecl* decl) {
    Type* type = new Type(decl->name(), TypeKind::Pstruct);
    type->setPstruct(decl);
    return type;
}

Type* TypeManager::defineReference(Type* inner) {
    assert(!inner->isReference());

    if (auto it = ref_types_.find(inner); it != ref_types_.end())
        return it->second;

    auto name = inner->declName()->str() + "&";
    Type* type = new Type(cc_.atom(name), TypeKind::Reference);
    type->setReference(inner);

    ref_types_.emplace(inner, type);
    return type;
}

Type* TypeManager::defineTypedef(Atom* name, Type* inner) {
    Type* type = new Type(name, TypeKind::Typedef);
    type->setTypedef(inner);
    return type;
}

Type* TypeManager::declareTypedef(Atom* name) {
    return new Type(name, TypeKind::Typedef);
}

FunctionType* TypeManager::defineFunction(QualType return_type,
                                          const std::vector<QualType>& args,
                                          bool variadic, FunctionType::Convention conv)
{
    FunctionCachePolicy::Lookup lookup{return_type, &args, variadic, conv};
    auto p = function_cache_.findForAdd(lookup);
    if (!p.found()) {
        auto ft = new FunctionType(return_type, args, variadic, conv);

        function_cache_.add(p, ft);
    }
    return *p;
}

FunctionType* TypeManager::UpdateReturnType(FunctionType* ft, QualType new_return_type) {
    std::vector<QualType> args;
    for (unsigned i = 0; i < ft->nargs(); i++)
        args.push_back(ft->arg_type(i));
    return defineFunction(new_return_type, args, ft->variadic(), ft->conv());
}

bool TypeManager::ArrayCachePolicy::matches(const Lookup& lookup, ArrayType* type) {
    return lookup.type == type->inner() && lookup.size == type->size() && lookup.is_flat == type->is_flat();
}

static inline uint32_t HashArrayType(Type* type, int size, bool is_flat) {
    auto first = ke::HashPointer(type);
    auto second = ke::HashInt32(size);
    auto third = ke::HashInt32(is_flat ? 1 : 0);
    return ke::HashCombine(ke::HashCombine(first, second), third);
}

uint32_t TypeManager::ArrayCachePolicy::hash(const Lookup& lookup) {
    return HashArrayType(lookup.type, lookup.size, lookup.is_flat);
}

TypenameInfo typeinfo_t::ToTypenameInfo() const {
    if (type)
        return TypenameInfo(type);
    return TypenameInfo(type_atom, is_label);
}

bool TypeManager::FunctionCachePolicy::matches(const Lookup& lookup, FunctionType* fun) {
    if (lookup.return_type != fun->return_type())
        return false;
    if (lookup.args->size() != fun->nargs())
        return false;
    for (unsigned int i = 0; i < fun->nargs(); i++) {
        if (lookup.args->at(i) != fun->arg_type(i))
            return false;
    }
    if (lookup.variadic != fun->variadic())
        return false;
    if (lookup.conv != fun->conv())
        return false;
    return true;
}

uint32_t TypeManager::FunctionCachePolicy::hash(const Lookup& lookup) {
    uint32_t h = lookup.return_type.hash();
    for (size_t i = 0; i < lookup.args->size(); i++)
        h = ke::HashCombine(h, lookup.args->at(i).hash());
    h = ke::HashCombine(h, ke::HashInt32(lookup.variadic));
    return h;
}

} // namespace cc
} // namespace sp
