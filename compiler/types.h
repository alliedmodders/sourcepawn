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

// Possible entries for "ident". These are used in the "symbol", "value"
// and arginfo structures. Not every constant is valid for every use.
// In an argument list, the list is terminated with a "zero" ident; labels
// cannot be passed as function arguments, so the value 0 is overloaded.
enum IdentifierKind {
    iINVALID = 0,
    iVARIABLE = 1,      /* cell that has an address and that can be fetched directly (lvalue) */
    iREFERENCE = 2,     /* iVARIABLE, but must be dereferenced */
    iARRAY = 3,
    iREFARRAY = 4,      /* an array passed by reference (i.e. a pointer) */
    iARRAYCELL = 5,     /* array element, cell that must be fetched indirectly */
    iARRAYCHAR = 6,     /* array element, character from cell from array */
    iEXPRESSION = 7,    /* expression result, has no address (rvalue) */
    iCONSTEXPR = 8,     /* constant expression (or constant symbol) */
    iFUNCTN = 9,
    iVARARGS = 11,      /* function specified ... as argument(s) */
    iACCESSOR = 13,     /* property accessor via a methodmap_method_t */
    iMETHODMAP = 14,    /* symbol defining a methodmap */
    iENUMSTRUCT = 15,   /* symbol defining an enumstruct */
    iSCOPE = 16,        /* local scope chain */
};

enum class TypeKind : uint8_t {
    Int,
    Object,
    Null,
    Function,
    Any,
    Void,
    Float,
    Bool,
    String,
    EnumStruct,
    Struct,
    Methodmap,
    Enum,
};

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
        ident(iINVALID),
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

    IdentifierKind ident : 6;  // Either iREFERENCE, iARRAY, or iVARIABLE.
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

struct structarg_t : public PoolObject
{
    structarg_t()
      : type(),
        name(nullptr),
        offs(0),
        index(0)
    {}

    typeinfo_t type;
    sp::Atom* name;
    unsigned int offs;
    int index;
};

class Type : public PoolObject
{
    friend class TypeDictionary;

  public:
    Type(sp::Atom* name, TypeKind kind);

    sp::Atom* name() const {
        return name_;
    }
    sp::Atom* nameAtom() const { return name_; }
    TypeKind kind() const { return kind_; }
    const char* kindName() const;
    const char* prettyName() const;
    int tagid() const {
        return value_;
    }

    bool isFixed() const {
        return fixed_;
    }

    template <class T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <class T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

    bool isStruct() const {
        return kind_ == TypeKind::Struct;
    }

    void setMethodmap(methodmap_t* map) {
        setFixed();
        assert(kind_ == TypeKind::Methodmap || kind_ == TypeKind::Enum);
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
        assert(kind_ == TypeKind::Function);
        funcenum_ptr_ = func;
    }
    void setObject() {
        setFixed();
        assert(kind_ == TypeKind::Object);
    }
    void setEnumStruct(symbol* sym) {
        setFixed();
        assert(kind_ == TypeKind::EnumStruct);
        enumstruct_ptr_ = sym;
    }
    void setFixed() {
        // This is separate from "kind_" because it persists across passes.
        fixed_ = true;
    }
    void set_tag(int tagid) {
        value_ = tagid;
    }

    void resetPtr();

  private:
    sp::Atom* name_;
    cell value_;
    bool fixed_;

    // These are reset in between the first and second passes, since the
    // underlying structures are reparsed.
    TypeKind kind_;
    union {
        funcenum_t* funcenum_ptr_;
        methodmap_t* methodmap_ptr_;
        symbol* enumstruct_ptr_;
        void* private_ptr_;
    };
};

class pstruct_t : public Type
{
  public:
    explicit pstruct_t(sp::Atom* name)
      : Type(name, TypeKind::Struct)
    {}

    const structarg_t* GetArg(sp::Atom* name) const;

    static bool is_a(Type* type) { return type->kind() == TypeKind::Struct; }

    PoolArray<structarg_t*> args;
};


class TypeDictionary
{
  public:
    explicit TypeDictionary(CompileContext& cc);

    Type* find(int tag);
    Type* find(sp::Atom* name);

    void init();

    Type* defineInt();
    Type* defineAny();
    Type* defineFunction(sp::Atom* name, funcenum_t* fe);
    Type* defineTypedef(const char* name, Type* other);
    Type* defineString();
    Type* defineFloat();
    Type* defineVoid();
    Type* defineObject(const char* name);
    Type* defineBool();
    Type* defineMethodmap(const char* name, methodmap_t* map);
    Type* defineEnumTag(const char* name);
    Type* defineEnumStruct(const char* name, symbol* sym);
    Type* defineTag(sp::Atom* atom);
    pstruct_t* definePStruct(sp::Atom* name);

    template <typename T>
    void forEachType(const T& callback) {
        for (const auto& type : types_)
            callback(type);
    }

    Type* type_nullfunc() const { return type_nullfunc_; }
    Type* type_object() const { return type_object_; }
    Type* type_null() const { return type_null_; }
    Type* type_function() const { return type_function_; }
    Type* type_any() const { return type_any_; }
    Type* type_void() const { return type_void_; }
    Type* type_float() const { return type_float_; }
    Type* type_bool() const { return type_bool_; }
    Type* type_string() const { return type_string_; }

    int tag_nullfunc() const { return type_nullfunc_->tagid(); }
    int tag_object() const { return type_object_->tagid(); }
    int tag_null() const { return type_null_->tagid(); }
    int tag_function() const { return type_function_->tagid(); }
    int tag_any() const { return type_any_->tagid(); }
    int tag_void() const { return type_void_->tagid(); }
    int tag_float() const { return type_float_->tagid(); }
    int tag_bool() const { return type_bool_->tagid(); }
    int tag_string() const { return type_string_->tagid(); }

  private:
    Type* add(const char* name, TypeKind kind);
    Type* add(sp::Atom* name, TypeKind kind);
    void RegisterType(Type* type);

  private:
    CompileContext& cc_;
    tr::unordered_map<sp::Atom*, Type*> types_;
    tr::unordered_map<int, Type*> tags_;
    Type* type_int_ = nullptr;
    Type* type_nullfunc_ = nullptr;
    Type* type_object_ = nullptr;
    Type* type_null_ = nullptr;
    Type* type_function_ = nullptr;
    Type* type_any_ = nullptr;
    Type* type_void_ = nullptr;
    Type* type_float_ = nullptr;
    Type* type_bool_ = nullptr;
    Type* type_string_ = nullptr;
};

const char* pc_tagname(int tag);

#endif // _INCLUDE_SOURCEPAWN_COMPILER_TYPES_H_
