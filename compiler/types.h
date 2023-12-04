/* vim: set sts=4 ts=8 sw=4 tw=99 et: */
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//  Copyright (c) AlliedModders LLC, 2023
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#pragma once

#include <memory>

#include <amtl/am-bits.h>
#include <amtl/am-enum.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <sp_vm_types.h>
#include "pool-objects.h"
#include "shared/string-atom.h"
#include "stl/stl-vector.h"

typedef int32_t cell;
typedef uint32_t ucell;

namespace sp {

using namespace cc;

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
};

enum class BuiltinType {
    Bool,
    Char,
    Int,
    Float,
    Null,
    Any,
    Void,
};

enum class TypeKind : uint8_t {
    Builtin,
    Object,
    Function,
    EnumStruct,
    Pstruct,
    Methodmap,
    Enum,
};

struct funcenum_t;
class EnumStructDecl;
class Expr;
class MethodmapDecl;
class PstructDecl;
class Type;

struct TypenameInfo {
    static constexpr uintptr_t kAtomFlag = 0x1;
    static constexpr uintptr_t kLabelFlag = 0x2;

    TypenameInfo() {}
    explicit TypenameInfo(Type* type) {
        impl.type = type;
    }
    explicit TypenameInfo(Atom* type_atom) {
        impl.atom = ke::SetPointerBits(type_atom, kAtomFlag);
    }
    TypenameInfo(Atom* type_atom, bool is_label) {
        impl.atom = ke::SetPointerBits(type_atom, kAtomFlag);
        if (is_label)
            impl.atom = ke::SetPointerBits(impl.atom, kLabelFlag);
    }

    union {
        Atom* atom;
        Type* type;
        void* raw;
    } impl;

    Type* type() const {
        assert(has_type());
        return ke::ClearPointerBits<2>(impl.type);
    }
    bool has_type() const {
        return ke::GetPointerBits<2>(impl.raw) == 0;
    }
    bool is_label() const {
        return ke::GetPointerBits<2>(impl.raw) == (kAtomFlag | kLabelFlag);
    }
    Atom* type_atom() const {
        assert((ke::GetPointerBits<2>(impl.raw) & kAtomFlag) == kAtomFlag);
        return ke::ClearPointerBits<2>(impl.atom);
    }
};

struct typeinfo_t {
    typeinfo_t()
      : type_atom(nullptr),
        type(nullptr),
        declared_type(nullptr),
        ident(iINVALID),
        is_const(false),
        is_new(false),
        has_postdims(false),
        is_label(false)
    {}

    // Array information.
    PoolList<int> dim_;

    // Either null or an array of size |numdim|, pool-allocated.
    PoolArray<Expr*> dim_exprs;

    // Type information.
    Atom* type_atom;    // Parsed atom.
    Type* type;

    // If non-null, this type was originally declared with this type, but was
    // rewritten for desugaring.
    Type* declared_type;

    IdentifierKind ident : 6;  // Either iREFERENCE, iARRAY, or iVARIABLE.
    bool is_const : 1;
    bool is_new : 1;        // New-style declaration.
    bool has_postdims : 1;  // Dimensions, if present, were in postfix position.
    bool is_label : 1;      // If type_atom came from a tLABEL.

    TypenameInfo ToTypenameInfo() const;

    int numdim() const { return (int)dim_.size(); }
    int dim(int i) const { return dim_[i]; }
    const PoolList<int>& dim_vec() const { return dim_; }

    void set_type(const TypenameInfo& rt) {
        if (rt.has_type()) {
            type_atom = nullptr;
            set_type(rt.type());
        } else {
            type_atom = rt.type_atom();
            is_label = rt.is_label();
            type = nullptr;
        }
    }

    void set_type(Type* t) { type = t; }

    Type* enum_struct_type() const;
    Type* semantic_type() const;
    bool is_implicit_dim(int i) const;
    bool isCharArray() const;
    Expr* get_dim_expr(int i) {
        assert(i < numdim());
        return (size_t)i < dim_exprs.size() ? dim_exprs[i] : nullptr;
    }
};

struct functag_t : public PoolObject
{
    functag_t()
     : ret_type(nullptr),
       args()
    {}
    Type* ret_type;
    PoolArray<typeinfo_t> args;
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
    Atom* name;
    unsigned int offs;
    int index;
};

class Type : public PoolObject
{
    friend class TypeManager;

  public:
    Type(Atom* name, TypeKind kind);

    Atom* name() const {
        return name_;
    }
    Atom* nameAtom() const { return name_; }
    TypeKind kind() const { return kind_; }
    const char* kindName() const;
    const char* prettyName() const;
    int type_index() const {
        return index_;
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

    bool isBuiltin() const { return kind_ == TypeKind::Builtin; }
    bool isBuiltin(BuiltinType type) const { return isBuiltin() && builtin_type_ == type; }
    bool isInt() const { return isBuiltin(BuiltinType::Int); }
    bool isNull() const { return isBuiltin(BuiltinType::Null); }
    bool isChar() const { return isBuiltin(BuiltinType::Char); }
    bool isAny() const { return isBuiltin(BuiltinType::Any); }
    bool isVoid() const { return isBuiltin(BuiltinType::Void); }
    bool isFloat() const { return isBuiltin(BuiltinType::Float); }
    bool isBool() const { return isBuiltin(BuiltinType::Bool); }

    bool coercesFromInt() const {
        if (kind_ == TypeKind::Enum)
            return true;
        if (kind_ != TypeKind::Builtin)
            return false;
        switch (builtin_type_) {
            case BuiltinType::Bool:
            case BuiltinType::Char:
            case BuiltinType::Int:
                return true;
        }
        return false;
    }

    void setMethodmap(MethodmapDecl* map) {
        assert(kind_ == TypeKind::Methodmap || kind_ == TypeKind::Enum);
        kind_ = TypeKind::Methodmap;
        methodmap_ptr_ = map;
    }

    bool isObject() const {
        return kind_ == TypeKind::Object || isNull();
    }

    bool isFunction() const {
        return kind_ == TypeKind::Function;
    }
    bool isCanonicalFunction() const {
        return isFunction() && !funcenum_ptr_;
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
    MethodmapDecl* asMethodmap() const {
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
    EnumStructDecl* asEnumStruct() const {
        if (!isEnumStruct())
            return nullptr;
        return enumstruct_ptr_;
    }

    bool isPstruct() const {
        return kind_ == TypeKind::Pstruct;
    }
    PstructDecl* asPstruct() const {
        if (!isPstruct())
            return nullptr;
        return pstruct_ptr_;
    }

  private:
    void setFunction(funcenum_t* func) {
        assert(kind_ == TypeKind::Function);
        funcenum_ptr_ = func;
    }
    void setObject() {
        assert(kind_ == TypeKind::Object);
    }
    void setEnumStruct(EnumStructDecl* decl) {
        assert(kind_ == TypeKind::EnumStruct);
        enumstruct_ptr_ = decl;
    }
    void setPstruct(PstructDecl* decl) {
        assert(kind_ == TypeKind::Pstruct);
        pstruct_ptr_ = decl;
    }
    void set_index(int index) {
        index_ = index;
    }

    void setBuiltinType(BuiltinType type) {
        assert(kind_ == TypeKind::Builtin);
        builtin_type_ = type;
    }

    void resetPtr();

  private:
    Atom* name_;
    int index_;
    TypeKind kind_;
    union {
        funcenum_t* funcenum_ptr_;
        MethodmapDecl* methodmap_ptr_;
        EnumStructDecl* enumstruct_ptr_;
        PstructDecl* pstruct_ptr_;
        BuiltinType builtin_type_;
    };
};

class TypeManager
{
  public:
    explicit TypeManager(CompileContext& cc);

    Type* Get(int index);

    Type* find(Atom* name);

    void init();

    Type* defineFunction(Atom* name, funcenum_t* fe);
    Type* defineTypedef(const char* name, Type* other);
    Type* defineObject(const char* name);
    Type* defineMethodmap(Atom* name, MethodmapDecl* map);
    Type* defineEnumTag(const char* name);
    Type* defineEnumStruct(Atom* name, EnumStructDecl* decl);
    Type* defineTag(Atom* atom);
    Type* definePstruct(PstructDecl* decl);

    Type* type_object() const { return type_object_; }
    Type* type_null() const { return type_null_; }
    Type* type_function() const { return type_function_; }
    Type* type_any() const { return type_any_; }
    Type* type_void() const { return type_void_; }
    Type* type_float() const { return type_float_; }
    Type* type_bool() const { return type_bool_; }
    Type* type_string() const { return type_string_; }
    Type* type_char() const { return type_string_; }
    Type* type_int() const { return type_int_; }

  private:
    Type* add(const char* name, TypeKind kind);
    Type* add(Atom* name, TypeKind kind);
    void RegisterType(Type* type);
    Type* defineBuiltin(const char* name, BuiltinType type);

  private:
    CompileContext& cc_;
    tr::unordered_map<Atom*, Type*> types_;
    std::vector<Type*> by_index_;
    Type* type_int_ = nullptr;
    Type* type_object_ = nullptr;
    Type* type_null_ = nullptr;
    Type* type_function_ = nullptr;
    Type* type_any_ = nullptr;
    Type* type_void_ = nullptr;
    Type* type_float_ = nullptr;
    Type* type_bool_ = nullptr;
    Type* type_string_ = nullptr;
};

} // namespace sp
