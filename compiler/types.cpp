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

using namespace ke;

TypeDictionary gTypes;

Type::Type(const char* name, cell value)
 : name_(name),
   value_(value)
{
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
TypeDictionary::findOrAdd(const char* name, int flags)
{
  for (const auto& type : types_) {
    if (strcmp(type->name(), name) == 0) {
      type->value() |= flags;
      return type.get();
    }
  }

  int tag = int(types_.length()) | flags;
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
TypeDictionary::init()
{
  Type* type = findOrAdd("_", 0);
  assert(type->value() == 0);

  type = findOrAdd("bool", 0);
  assert(type->value() == 1);
}
