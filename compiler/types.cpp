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

Type::Type(sp::Atom* name, cell value)
 : name_(name),
   value_(value),
   fixed_(0),
   intrinsic_(false),
   first_pass_kind_(TypeKind::None),
   kind_(TypeKind::None)
{
    private_ptr_ = nullptr;
}

void
Type::resetPtr()
{
    // We try to persist tag information across passes, since globals are
    // preserved and core types should be too. However user-defined types
    // that attach extra structural information are cleared, as that
    // data is not retained into the statWRITE pass.
    if (intrinsic_)
        return;

    if (kind_ != TypeKind::None)
        first_pass_kind_ = kind_;
    kind_ = TypeKind::None;
    private_ptr_ = nullptr;
}

bool
Type::isDeclaredButNotDefined() const
{
    if (kind_ != TypeKind::None)
        return false;
    if (first_pass_kind_ == TypeKind::None || first_pass_kind_ == TypeKind::EnumStruct) {
        return true;
    }
    return false;
}

const char*
Type::prettyName() const
{
  if (kind_ == TypeKind::Function)
    return kindName();
  if (tagid() == 0)
    return "int";
  return name();
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

bool
Type::isLabelTag() const
{
    auto types = CompileContext::get().types();
    if (tagid() == 0 || tagid() == types->tag_bool() || tagid() == types->tag_float())
        return false;
    return kind_ == TypeKind::None;
}

TypeDictionary::TypeDictionary(CompileContext& cc)
  : cc_(cc)
{}

Type*
TypeDictionary::find(sp::Atom* name)
{
    for (const auto& type : types_) {
        if (type->nameAtom() == name)
            return type;
    }
    return nullptr;
}

Type*
TypeDictionary::find(int tag)
{
    assert(size_t(tag) < types_.size());

    return types_[tag];
}

Type*
TypeDictionary::findOrAdd(const char* name)
{
    sp::Atom* atom = cc_.atom(name);
    for (const auto& type : types_) {
        if (type->nameAtom() == atom)
            return type;
    }

    int tag = int(types_.size());
    Type* type = new Type(atom, tag);
    types_.push_back(type);
    return types_.back();
}

void
TypeDictionary::clear()
{
    types_.clear();
}

void
TypeDictionary::clearExtendedTypes()
{
    for (const auto& type : types_)
        type->resetPtr();
}

void
TypeDictionary::init()
{
    Type* type = findOrAdd("_");
    assert(type->tagid() == 0);

    type = defineBool();
    assert(type->tagid() == 1);

    tag_bool_ = type->tagid();
    tag_any_ = defineAny()->tagid();
    tag_function_ = defineFunction("Function", nullptr)->tagid();
    tag_string_ = defineString()->tagid();
    tag_float_ = defineFloat()->tagid();
    tag_void_ = defineVoid()->tagid();
    tag_object_ = defineObject("object")->tagid();
    tag_null_ = defineObject("null_t")->tagid();
    tag_nullfunc_ = defineObject("nullfunc_t")->tagid();

    for (const auto& type : types_)
        type->setIntrinsic();
}

Type*
TypeDictionary::defineAny()
{
    return findOrAdd("any");
}

Type*
TypeDictionary::defineFunction(const char* name, funcenum_t* fe)
{
    Type* type = findOrAdd(name);
    type->setFunction(fe);
    return type;
}

Type*
TypeDictionary::defineString()
{
    Type* type = findOrAdd("String");
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineFloat()
{
    Type* type = findOrAdd("Float");
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineVoid()
{
    Type* type = findOrAdd("void");
    type->setFixed();
    return type;
}

Type*
TypeDictionary::defineObject(const char* name)
{
    Type* type = findOrAdd(name);
    type->setObject();
    return type;
}

Type*
TypeDictionary::defineBool()
{
    return findOrAdd("bool");
}

Type*
TypeDictionary::defineMethodmap(const char* name, methodmap_t* map)
{
    Type* type = findOrAdd(name);
    type->setMethodmap(map);
    return type;
}

Type*
TypeDictionary::defineEnumTag(const char* name)
{
    Type* type = findOrAdd(name);
    type->setEnumTag();
    if (isupper(*name))
        type->setFixed();
    return type;
}

Type*
TypeDictionary::defineEnumStruct(const char* name, symbol* sym)
{
    Type* type = findOrAdd(name);
    type->setEnumStruct(sym);
    return type;
}

Type*
TypeDictionary::defineTag(const char* name)
{
    Type* type = findOrAdd(name);
    if (isupper(*name))
        type->setFixed();
    return type;
}

Type*
TypeDictionary::definePStruct(const char* name, pstruct_t* ps)
{
    Type* type = findOrAdd(name);
    type->setStruct(ps);
    return type;
}

const char*
pc_tagname(int tag)
{
    auto types = CompileContext::get().types();
    if (Type* type = types->find(tag))
        return type->name();
    return "__unknown__";
}

bool
typeinfo_t::isCharArray() const
{
    auto types = CompileContext::get().types();
    return numdim() == 1 && tag() == types->tag_string();
}
