/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
/*  Pawn compiler
 *
 *  Function and variable definition and declaration, statement parser.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
 *  Copyright (c) AlliedModders LLC 2024
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 */
#include <ctype.h>

#include <utility>

#include <amtl/am-string.h>
#include "array-helpers.h"
#include "compile-context.h"
#include "parse-node.h"
#include "sc.h"
#include "sctracker.h"
#include "types.h"

namespace sp {
namespace cc {

using namespace ke;

Type::Type(Atom* name, TypeKind kind)
 : name_(name),
   index_(-1),
   kind_(kind)
{
}

const char* Type::prettyName() {
  if (kind_ == TypeKind::Function)
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
    case TypeKind::Pstruct:
      return "struct";
    case TypeKind::Methodmap:
      return "methodmap";
    case TypeKind::Enum:
      return "enum";
    case TypeKind::Object:
      return "object";
    case TypeKind::Function:
      if (funcenum_ptr_) {
        if (funcenum_ptr_->entries.size() > 1)
          return "typeset";
        if (ke::StartsWith(name_->chars(), "::"))
          return "function";
        return "function";
      }
      return "function";
    default:
      return "type";
  }
}

bool Type::canOperatorOverload() const {
    return isEnum() || isMethodmap() || isFloat() || isInt();
}

bool Type::isCharArray() const {
    return isArray() && inner()->isChar();
}

cell_t Type::CellStorageSize() {
    if (auto at = as<ArrayType>())
        return CalcArraySize(at);
    if (auto es = asEnumStruct())
        return es->array_size();
    return 1;
}

ArrayType::ArrayType(Type* inner, int size)
  : Type(nullptr, TypeKind::Array)
{
    inner_type_ = inner;
    size_ = size;
}

TypeManager::TypeManager(CompileContext& cc)
  : cc_(cc)
{
    array_cache_.init(256);
    function_cache_.init(512);
}

Type* TypeManager::find(Atom* atom) {
    auto iter = types_.find(atom);
    if (iter == types_.end())
        return nullptr;
    return iter->second;
}

Type* TypeManager::Get(int index) {
    return by_index_[index];
}

Type* TypeManager::add(const char* name, TypeKind kind) {
    return add(cc_.atom(name), kind);
}

Type* TypeManager::add(Atom* name, TypeKind kind) {
    Type* type = new Type(name, kind);
    RegisterType(type);
    return type;
}

void TypeManager::RegisterType(Type* type, bool unique_name) {
    type->set_index((int)by_index_.size());
    by_index_.emplace_back(type);

    if (unique_name) {
        assert(types_.find(type->declName()) == types_.end());
        types_.emplace(type->declName(), type);
    }
}

Type* TypeManager::defineBuiltin(const char* name, BuiltinType type) {
    Type* ptr = add(name, TypeKind::Builtin);
    ptr->setBuiltinType(type);
    return ptr;
}

ArrayType* TypeManager::defineArray(Type* element_type, int dim) {
    return defineArray(element_type, &dim, 1);
}

ArrayType* TypeManager::defineArray(Type* element_type, const PoolArray<int>& dim_vec) {
    return defineArray(element_type, dim_vec.buffer(), (int)dim_vec.size());
}

ArrayType* TypeManager::defineArray(Type* element_type, const int* dim_vec, int numdim) {
    assert(!element_type->isArray());
    assert(numdim >= 1);

    size_t depth = numdim - 1;
    Type* iter = element_type;
    for (;;) {
        auto lookup = ArrayCachePolicy::Lookup{iter, dim_vec[depth]};
        auto p = array_cache_.findForAdd(lookup);
        if (!p.found()) {
            auto at = new ArrayType(iter, dim_vec[depth]);
            RegisterType(at, false);

            array_cache_.add(p, at);
        }
        iter = *p;

        if (!depth)
            break;
        depth--;
    }
    return iter->to<ArrayType>();
}

ArrayType* TypeManager::redefineArray(Type* element_type, ArrayType* old_type) {
    std::vector<int> dim_vec;
    for (auto iter = old_type; iter; iter = iter->inner()->as<ArrayType>())
        dim_vec.emplace_back(iter->size());
    return defineArray(element_type, dim_vec.data(), (int)dim_vec.size());
}

void TypeManager::init() {
    type_int_ = defineBuiltin("int", BuiltinType::Int);
    types_.emplace(cc_.atom("_"), type_int_);

    type_bool_ = defineBuiltin("bool", BuiltinType::Bool);
    type_any_ = defineBuiltin("any", BuiltinType::Any);

    type_float_ = defineBuiltin("float", BuiltinType::Float);
    types_.emplace(cc_.atom("Float"), type_float_);

    type_void_ = defineBuiltin("void", BuiltinType::Void);
    type_null_ = defineBuiltin("null_t", BuiltinType::Null);

    type_string_ = defineBuiltin("char", BuiltinType::Char);
    types_.emplace(cc_.atom("String"), type_string_);

    type_function_ = defineFunction(cc_.atom("Function"), nullptr);
    type_object_ = defineObject("object");
}

Type* TypeManager::defineFunction(Atom* name, funcenum_t* fe) {
    Type* type = add(name, TypeKind::Function);
    type->setFunction(fe);
    return type;
}

Type* TypeManager::defineObject(const char* name) {
    Type* type = add(name, TypeKind::Object);
    type->setObject();
    return type;
}

Type* TypeManager::defineMethodmap(Atom* name, MethodmapDecl* map) {
    Type* type = find(name);
    if (!type)
        type = add(name, TypeKind::Methodmap);
    type->setMethodmap(map);
    return type;
}

Type*
TypeManager::defineEnumTag(const char* name)
{
    auto atom = cc_.atom(name);
    if (auto type = find(atom)) {
        assert(type->kind() == TypeKind::Methodmap);
        return type;
    }

    Type* type = add(atom, TypeKind::Enum);
    return type;
}

Type* TypeManager::defineEnumStruct(Atom* name, EnumStructDecl* decl) {
    Type* type = add(name, TypeKind::EnumStruct);
    type->setEnumStruct(decl);
    return type;
}

Type*
TypeManager::defineTag(Atom* name) {
    Type* type = add(name, TypeKind::Enum);
    return type;
}

Type* TypeManager::definePstruct(PstructDecl* decl) {
    assert(find(decl->name()) == nullptr);

    Type* type = add(decl->name(), TypeKind::Pstruct);
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
    RegisterType(type, false);

    ref_types_.emplace(inner, type);
    return type;
}

FunctionType* TypeManager::defineFunction(Type* return_type,
                                          const std::vector<std::pair<QualType, sp::Atom*>>& args,
                                          bool variadic)
{
    FunctionCachePolicy::Lookup lookup{return_type, &args, variadic};
    auto p = function_cache_.findForAdd(lookup);
    if (!p.found()) {
        auto ft = new FunctionType(return_type, args, variadic);
        RegisterType(ft, false);

        function_cache_.add(p, ft);
    }
    return *p;
}

bool TypeManager::ArrayCachePolicy::matches(const Lookup& lookup, ArrayType* type) {
    return lookup.type == type->inner() && lookup.size == type->size();
}

static inline uint32_t HashArrayType(Type* type, int size) {
    auto first = ke::HashPointer(type);
    auto second = ke::HashInt32(size);
    return ke::HashCombine(first, second);
}

uint32_t TypeManager::ArrayCachePolicy::hash(const Lookup& lookup) {
    return HashArrayType(lookup.type, lookup.size);
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
        if (lookup.args->at(i).first != fun->arg_type(i))
            return false;
        if (lookup.args->at(i).second != fun->arg_name(i))
            return false;
    }
    return true;
}

uint32_t TypeManager::FunctionCachePolicy::hash(const Lookup& lookup) {
    uint32_t h = ke::HashPointer(lookup.return_type);
    for (size_t i = 0; i < lookup.args->size(); i++) {
        h = ke::HashCombine(h, lookup.args->at(i).first.hash());
        h = ke::HashCombine(h, ke::HashPointer(lookup.args->at(i).second));
    }
    h = ke::HashCombine(h, ke::HashInt32(lookup.variadic));
    return h;
}

} // namespace cc
} // namespace sp
