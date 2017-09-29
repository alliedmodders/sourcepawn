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

#define TAGTYPEMASK   (0x3E000000)
#define TAGFLAGMASK   (TAGTYPEMASK | 0x40000000)
#define TAGID(tag)    ((tag) & ~(TAGFLAGMASK))

enum class TypeKind : uint32_t
{
  None,
  Struct    = 0x02000000,
  Methodmap = 0x04000000,
  Enum      = 0x08000000,
  Object    = 0x10000000,
  Function  = 0x20000000
};
KE_DEFINE_ENUM_OPERATORS(TypeKind)

struct pstruct_t;
struct funcenum_t;
struct methodmap_t;

class Type
{
  friend class TypeDictionary;

public:
  Type(const char* name, cell value);

  const char* name() const {
    return name_.chars();
  }
  TypeKind kind() const {
    return kind_;
  }
  cell value() const {
    return value_ | int(kind_) | fixed_;
  }
  int tagid() const {
    return TAGID(value_);
  }

  bool isDefinedType() const {
    return kind_ != TypeKind::None ||
           (value() & TAGTYPEMASK) != 0;
  }

  bool isFixed() const {
    return !!fixed_;
  }

  bool isStruct() const {
    return kind_ == TypeKind::Struct;
  }
  pstruct_t* asStruct() const {
    if (!isStruct())
      return nullptr;
    return pstruct_ptr_;
  }

  void setMethodmap(methodmap_t* map) {
    setFixed();
    kind_ = TypeKind::Methodmap;
    methodmap_ptr_ = map;
  }

  bool isObject() const {
    return kind_ == TypeKind::Object;
  }

  bool isFunction() const {
    return kind_ == TypeKind::Function;
  }
  funcenum_t* asFunction() const {
    if (!isFunction())
      return nullptr;
    return funcenum_ptr_;
  }
  // This can return null if it's the Function type.
  funcenum_t* toFunction() const {
    assert(isFunction());
    return funcenum_ptr_;
  }

  bool isMethodmap() const {
    return kind_ == TypeKind::Methodmap;
  }
  methodmap_t* asMethodmap() const {
    if (!isMethodmap())
      return nullptr;
    return methodmap_ptr_;
  }

private:
  void setFunction(funcenum_t* func) {
    setFixed();
    kind_ = TypeKind::Function;
    funcenum_ptr_ = func;
  }
  void setObject() {
    setFixed();
    kind_ = TypeKind::Object;
  }
  void setEnumTag() {
    kind_ = TypeKind::Enum;
  }
  void setStruct(pstruct_t* ptr) {
    setFixed();
    kind_ = TypeKind::Struct;
    pstruct_ptr_ = ptr;
  }
  void setFixed() {
    // This is separate from "kind_" because it persists across passes.
    fixed_ = 0x40000000;
  }
  void setIntrinsic() {
    intrinsic_ = true;
  }

  void resetPtr();

private:
  ke::AString name_;
  cell value_;
  int fixed_;
  bool intrinsic_;

  // These are reset in between the first and second passes, since the
  // underlying structures are reparsed.
  TypeKind kind_;
  union {
    pstruct_t* pstruct_ptr_;
    funcenum_t* funcenum_ptr_;
    methodmap_t* methodmap_ptr_;
    void* private_ptr_;
  };
};

class TypeDictionary
{
public:
  TypeDictionary();

  Type* find(int tag);
  Type* find(const char* name);
  Type* findByValue(int tag);

  void clearExtendedTypes();

  void reset();
  void init();

  Type* defineAny();
  Type* defineFunction(const char* name, funcenum_t* fe);
  Type* defineTypedef(const char* name, Type* other);
  Type* defineString();
  Type* defineFloat();
  Type* defineVoid();
  Type* defineObject(const char* name);
  Type* defineBool();
  Type* defineMethodmap(const char* name, methodmap_t* map);
  Type* defineEnumTag(const char* name);
  Type* defineTag(const char* name);
  Type* definePStruct(const char* name, pstruct_t* ps);

  template <typename T>
  void forEachType(const T& callback) {
    for (const auto& type : types_)
      callback(type.get());
  }

private:
  Type* findOrAdd(const char* name);

private:
  ke::Vector<ke::UniquePtr<Type>> types_;
};

extern TypeDictionary gTypes;

#endif // _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
