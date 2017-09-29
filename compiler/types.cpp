/* vim: set sts=2 ts=8 sw=2 tw=99 et: */
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
#include "types.h"
#include "sc.h"
#include <ctype.h>

using namespace ke;

TypeDictionary gTypes;

Type::Type(const char* name, cell value)
 : name_(name),
   value_(value),
   fixed_(0),
   intrinsic_(false),
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

  // We preserve funcenums across compilations, but not the private ptr,
  // since dotypedef() and dotypeset() want to check that the type
  // wasn't previously defined as something else.
  if (kind_ != TypeKind::Function)
    kind_ = TypeKind::None;
  private_ptr_ = nullptr;
}

TypeDictionary::TypeDictionary()
{
}

Type*
TypeDictionary::find(const char* name)
{
  for (const auto& type : types_) {
    if (strcmp(type->name(), name) == 0)
      return type.get();
  }
  return nullptr;
}

Type*
TypeDictionary::find(int tag)
{
  int index = TAGID(tag);
  assert(size_t(index) < types_.length());

  return types_[index].get();
}

Type*
TypeDictionary::findByValue(int tag)
{
  int tagid = TAGID(tag);
  Type* type = find(tagid);
  if (type && type->value() == tag)
    return type;
  return nullptr;
}

Type*
TypeDictionary::findOrAdd(const char* name)
{
  for (const auto& type : types_) {
    if (strcmp(type->name(), name) == 0)
      return type.get();
  }

  int tag = int(types_.length());
  UniquePtr<Type> type = MakeUnique<Type>(name, tag);
  types_.append(Move(type));
  return types_.back().get();
}

void
TypeDictionary::reset()
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
  assert(type->value() == 0);

  type = findOrAdd("bool");
  assert(type->value() == 1);

  pc_anytag = defineAny()->value();
  pc_functag = defineFunction("Function", nullptr)->value();
  pc_tag_string = defineString()->value();
  sc_rationaltag = defineFloat()->value();
  pc_tag_void = defineVoid()->value();
  pc_tag_object = defineObject("object")->value();
  pc_tag_bool = defineBool()->value();
  pc_tag_null_t = defineObject("null_t")->value();
  pc_tag_nullfunc_t = defineObject("nullfunc_t")->value();

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
TypeDictionary::defineMethodmap(const char* name)
{
  Type* type = findOrAdd(name);
  type->setMethodmap();
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
