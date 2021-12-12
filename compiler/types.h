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
#include "pool-objects.h"
#include "shared/string-atom.h"
#include "stl/stl-vector.h"

typedef int32_t cell;
typedef uint32_t ucell;

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
struct symbol;
class Expr;

struct TypenameInfo {
    TypenameInfo() {}
    explicit TypenameInfo(int tag) : resolved_tag(tag) {}
    explicit TypenameInfo(sp::Atom* type_atom) : type_atom(type_atom) {}
    TypenameInfo(sp::Atom* type_atom, bool is_label) : type_atom(type_atom) {
        if (is_label)
            set_is_label();
    }

    sp::Atom* type_atom = nullptr;
    int resolved_tag = -1;

    int tag() const {
        assert(has_tag());
        return resolved_tag;
    }
    bool has_tag() const { return resolved_tag >= 0; }

    void set_is_label() {
        assert(type_atom);
        assert(resolved_tag == -1);
        resolved_tag = -2;
    }
    bool is_label() const { return resolved_tag == -2; }
};

struct typeinfo_t {
    typeinfo_t()
      : type_atom(nullptr),
        tag_(-1),
        declared_tag(0),
        ident(0),
        is_const(false),
        is_new(false),
        has_postdims(false),
        is_label(false)
    {}

    // Array information.
    PoolList<int> dim;

    // Either null or an array of size |numdim|, pool-allocated.
    PoolArray<Expr*> dim_exprs;

    // Type information.
    sp::Atom* type_atom;    // Parsed atom.
    int tag_;               // Effective tag.

    // If non-zero, this type was originally declared with this type, but was
    // rewritten for desugaring.
    int declared_tag;

    int ident : 5;          // Either iREFERENCE, iARRAY, or iVARIABLE.
    bool is_const : 1;
    bool is_new : 1;        // New-style declaration.
    bool has_postdims : 1;  // Dimensions, if present, were in postfix position.
    bool is_label : 1;      // If type_atom came from a tLABEL.

    TypenameInfo ToTypenameInfo() const {
        if (tag_ >= 0)
            return TypenameInfo(tag_);
        return TypenameInfo(type_atom, is_label);
    }

    void set_type(const TypenameInfo& rt) {
        if (rt.resolved_tag >= 0) {
            type_atom = nullptr;
            set_tag(rt.tag());
        } else {
            assert(rt.type_atom);
            type_atom = rt.type_atom;
            is_label = rt.is_label();
            tag_ = -1;
        }
    }

    int tag() const {
        assert(tag_ >= 0);
        return tag_;
    }
    void set_tag(int tag) { tag_ = tag; }
    bool has_tag() const { return tag_ >= 0; }

    int enum_struct_tag() const {
        return tag() ? 0 : declared_tag;
    }
    int semantic_tag() const {
        return tag() ? tag() : declared_tag;
    }
    bool is_implicit_dim(int i) const {
        return semantic_tag() != tag() && i == numdim() - 1;
    }
    int numdim() const { return (int)dim.size(); }
    bool isCharArray() const;
    Expr* get_dim_expr(int i) {
        assert(i < numdim());
        return (size_t)i < dim_exprs.size() ? dim_exprs[i] : nullptr;
    }
};

struct funcarg_t {
    typeinfo_t type;
};

struct functag_t : public PoolObject
{
    functag_t()
     : ret_tag(0),
       args()
    {}
    int ret_tag;
    PoolArray<funcarg_t> args;
};

class Type : public PoolObject
{
    friend class TypeDictionary;

  public:
    Type(sp::Atom* name, cell value);

    const char* name() const {
        return name_->chars();
    }
    sp::Atom* nameAtom() const { return name_; }
    TypeKind kind() const { return kind_; }
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

    bool isLabelTag() const;
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
    sp::Atom* name_;
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
    Type* find(sp::Atom* name);

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
            callback(type);
    }

    int tag_nullfunc() const { return tag_nullfunc_; }
    int tag_object() const { return tag_object_; }
    int tag_null() const { return tag_null_; }
    int tag_function() const { return tag_function_; }
    int tag_any() const { return tag_any_; }
    int tag_void() const { return tag_void_; }

  private:
    Type* findOrAdd(const char* name);

  private:
    tr::vector<Type*> types_;
    int tag_nullfunc_ = -1;
    int tag_object_ = -1;
    int tag_null_ = -1;
    int tag_function_ = -1;
    int tag_any_ = -1;
    int tag_void_ = -1;
};

const char* pc_tagname(int tag);

extern TypeDictionary gTypes;

#endif // _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
