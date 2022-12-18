/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
/*  Pawn compiler
 *
 *  Function and variable definition and declaration, statement parser.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
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
 *
 *  Version: $Id$
 */
#include <ctype.h>

#include <utility>

#include "compile-context.h"
#include "sc.h"
#include "sctracker.h"
#include "types.h"

using namespace ke;

Type::Type(sp::Atom* name, TypeKind kind)
 : name_(name),
   value_(0),
   fixed_(0),
   kind_(kind)
{
    private_ptr_ = nullptr;
}

const char*
Type::prettyName() const
{
  if (kind_ == TypeKind::Function)
    return kindName();
  if (tagid() == 0)
    return "int";
  return name()->chars();
}

const char*
Type::kindName() const
{
  switch (kind_) {
    case TypeKind::EnumStruct:
      return "enum struct";
    case TypeKind::Struct:
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
        return "typedef";
      }
      return "function";
    default:
      return "type";
  }
}

TypeDictionary::TypeDictionary(CompileContext& cc)
  : cc_(cc)
{}

Type* TypeDictionary::find(sp::Atom* atom) {
    auto iter = types_.find(atom);
    if (iter == types_.end())
        return nullptr;
    return iter->second;
}

Type* TypeDictionary::find(int tag) {
    auto iter = tags_.find(tag);
    assert(iter != tags_.end());
    return iter->second;
}

Type* TypeDictionary::add(const char* name, TypeKind kind) {
    return add(cc_.atom(name), kind);
}

Type* TypeDictionary::add(sp::Atom* name, TypeKind kind) {
    Type* type = new Type(name, kind);
    RegisterType(type);
    return type;
}

void TypeDictionary::RegisterType(Type* type) {
    assert(types_.find(type->name()) == types_.end());

    int tag = int(types_.size());
    type->set_tag(tag);
    types_.emplace(type->name(), type);
    tags_.emplace(tag, type);
}

void
TypeDictionary::init()
{
    type_int_ = add("_", TypeKind::Int);
    type_bool_ = defineBool();
    type_any_ = defineAny();
    type_function_ = defineFunction(cc_.atom("Function"), nullptr);
    type_string_ = defineString();
    type_float_ = defineFloat();
    type_void_ = defineVoid();
    type_object_ = defineObject("object");
    type_null_ = defineObject("null_t");
    type_nullfunc_ = defineObject("nullfunc_t");
}

Type*
TypeDictionary::defineAny()
{
    return add("any", TypeKind::Any);
}

Type* TypeDictionary::defineFunction(sp::Atom* name, funcenum_t* fe) {
    Type* type = add(name, TypeKind::Function);
    type->setFunction(fe);
    return type;
}

Type*
TypeDictionary::defineString()
{
    Type* type = add("String", TypeKind::String);
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineFloat()
{
    Type* type = add("Float", TypeKind::Float);
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineVoid()
{
    Type* type = add("void", TypeKind::Void);
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineObject(const char* name)
{
    Type* type = add(name, TypeKind::Object);
    type->setObject();
    return type;
}

Type*
TypeDictionary::defineBool()
{
    return add("bool", TypeKind::Bool);
}

Type*
TypeDictionary::defineMethodmap(const char* name, methodmap_t* map)
{
    auto atom = cc_.atom(name);
    Type* type = find(atom);
    if (!type)
        type = add(atom, TypeKind::Methodmap);
    type->setMethodmap(map);
    return type;
}

Type*
TypeDictionary::defineEnumTag(const char* name)
{
    auto atom = cc_.atom(name);
    if (auto type = find(atom)) {
        assert(type->kind() == TypeKind::Methodmap);
        return type;
    }

    Type* type = add(atom, TypeKind::Enum);
    if (isupper(*name))
        type->setFixed();
    return type;
}

Type*
TypeDictionary::defineEnumStruct(const char* name, symbol* sym)
{
    Type* type = add(name, TypeKind::EnumStruct);
    type->setEnumStruct(sym);
    return type;
}

Type*
TypeDictionary::defineTag(sp::Atom* name) {
    Type* type = add(name, TypeKind::Enum);
    if (isupper(*name->chars()))
        type->setFixed();
    return type;
}

pstruct_t* TypeDictionary::definePStruct(sp::Atom* name) {
    assert(find(name) == nullptr);

    pstruct_t* type = new pstruct_t(name);
    RegisterType(type);
    return type;
}

const char*
pc_tagname(int tag)
{
    auto types = CompileContext::get().types();
    if (Type* type = types->find(tag))
        return type->name()->chars();
    return "__unknown__";
}

bool
typeinfo_t::isCharArray() const
{
    auto types = CompileContext::get().types();
    return numdim() == 1 && tag() == types->tag_string();
}

const structarg_t* pstruct_t::GetArg(sp::Atom* name) const {
    for (const auto& arg : args) {
        if (arg->name == name)
            return arg;
    }
    return nullptr;
}

