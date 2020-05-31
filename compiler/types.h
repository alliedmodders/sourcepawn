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
#ifndef _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
#define _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_

#include <memory>

#include <amtl/am-enum.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <sp_vm_types.h>
#include "amx.h"
#include "pool-allocator.h"

#define TAGTYPEMASK (0x3E000000)
#define TAGFLAGMASK (TAGTYPEMASK | 0x40000000)

enum class TypeKind : uint32_t {
    None,
    EnumStruct = 0x01000000,
    Struct = 0x02000000,
    Methodmap = 0x04000000,
    Enum = 0x08000000,
    Object = 0x10000000,
    Function = 0x20000000
};
KE_DEFINE_ENUM_OPERATORS(TypeKind)

struct pstruct_t;
struct funcenum_t;
struct methodmap_t;
struct constvalue;
struct symbol;

struct typeinfo_t {
    // Array information.
    int numdim;
    int dim[sDIMEN_MAX];
    int idxtag[sDIMEN_MAX];
    cell size;
    constvalue* enumroot;

    // Type information.
    int tag;           // Effective tag.
    int ident;         // Either iREFERENCE, iARRAY, or iVARIABLE.
    bool is_const : 1;
    bool is_new : 1;        // New-style declaration.
    bool has_postdims : 1;  // Dimensions, if present, were in postfix position.

    // If non-zero, this type was originally declared with this type, but was
    // rewritten for desugaring.
    int declared_tag;

    int semantic_tag() const {
        return tag ? tag : declared_tag;
    }
    bool isCharArray() const;
};

struct funcarg_t {
    int tag;
    int dimcount;
    int dims[sDIMEN_MAX];
    int ident;
    bool fconst : 1;
};

struct functag_t : public PoolObject
{
    functag_t()
     : ret_tag(0),
       args()
    {}
    int ret_tag;
    PoolList<funcarg_t> args;
};

class Type
{
    friend class TypeDictionary;

  public:
    Type(const char* name, cell value);

    const char* name() const {
        return name_.c_str();
    }
    TypeKind kind() const {
        return kind_;
    }
    const char* kindName() const;
    const char* prettyName() const;
    cell smx_export_value() const {
        return value_ | int(kind_) | fixed_;
    }
    int tagid() const {
        return value_;
    }

    bool isDeclaredButNotDefined() const;
    bool isDefinedType() const {
        return kind_ != TypeKind::None;
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

    bool isEnum() const {
        return kind_ == TypeKind::Enum;
    }

    bool isEnumStruct() const {
        return kind_ == TypeKind::EnumStruct;
    }
    symbol* asEnumStruct() const {
        if (!isEnumStruct())
            return nullptr;
        return enumstruct_ptr_;
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
    void setEnumStruct(symbol* sym) {
        setFixed();
        kind_ = TypeKind::EnumStruct;
        enumstruct_ptr_ = sym;
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
    std::string name_;
    cell value_;
    int fixed_;
    bool intrinsic_;
    TypeKind first_pass_kind_;

    // These are reset in between the first and second passes, since the
    // underlying structures are reparsed.
    TypeKind kind_;
    union {
        pstruct_t* pstruct_ptr_;
        funcenum_t* funcenum_ptr_;
        methodmap_t* methodmap_ptr_;
        symbol* enumstruct_ptr_;
        void* private_ptr_;
    };
};

class TypeDictionary
{
  public:
    TypeDictionary();

    Type* find(int tag);
    Type* find(const char* name);

    void init();
    void clearExtendedTypes();
    void clear();

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
    Type* defineEnumStruct(const char* name, symbol* sym);
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
    std::vector<std::unique_ptr<Type>> types_;
};

extern TypeDictionary gTypes;

#endif // _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
