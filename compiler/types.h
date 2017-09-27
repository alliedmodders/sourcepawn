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
#ifndef _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
#define _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_

#include <amtl/am-string.h>
#include <amtl/am-uniqueptr.h>
#include <amtl/am-vector.h>
#include "amx.h"

#define FIXEDTAG     0x40000000
#define FUNCTAG      0x20000000
#define OBJECTTAG    0x10000000
#define ENUMTAG      0x08000000
#define METHODMAPTAG 0x04000000
#define TAGTYPEMASK   (FUNCTAG | OBJECTTAG | ENUMTAG | METHODMAPTAG)
#define TAGFLAGMASK   (FIXEDTAG | TAGTYPEMASK)
#define TAGID(tag)    ((tag) & ~(TAGFLAGMASK))

enum class TypeKind : uint32_t
{
  None,
  Struct = 0x02000000
};
KE_DEFINE_ENUM_OPERATORS(TypeKind)

struct pstruct_t;

class Type
{
public:
  Type(const char* name, cell value);

  const char* name() const {
    return name_.chars();
  }
  TypeKind kind() const {
    return kind_;
  }
  cell& value() {
    return value_;
  }
  cell value() const {
    return value_ | int(kind_);
  }
  int tagid() const {
    return TAGID(value_);
  }

  void resetPtr() {
    kind_ = TypeKind::None;
    private_ptr_ = nullptr;
  }

  pstruct_t* asStruct() const {
    assert(kind_ == TypeKind::Struct);
    return pstruct_ptr_;
  }
  void setStruct(pstruct_t* ptr) {
    kind_ = TypeKind::Struct;
    pstruct_ptr_ = ptr;
  }

private:
  ke::AString name_;
  cell value_;

  // These are reset in between the first and second passes, since the
  // underlying structures are reparsed.
  TypeKind kind_;
  union {
    pstruct_t* pstruct_ptr_;
    void* private_ptr_;
  };
};

class TypeDictionary
{
public:
  TypeDictionary();

  Type* find(int tag);
  Type* find(const char* name);
  Type* findOrAdd(const char* name, int flags);
  Type* findByValue(int tag);

  void clearExtendedTypes();

  void reset();
  void init();

  template <typename T>
  void forEachType(const T& callback) {
    for (const auto& type : types_)
      callback(type.get());
  }

private:
  ke::Vector<ke::UniquePtr<Type>> types_;
};

extern TypeDictionary gTypes;

#endif // _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
