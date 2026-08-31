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
#include <optional>

#include <amtl/am-bits.h>
#include <amtl/am-enum.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <sp_vm_types.h>
#include "pool-objects.h"
#include "utils/string-atom.h"
#include "stl/stl-vector.h"

typedef int32_t cell;
typedef uint32_t ucell;

namespace sp {
namespace cc {

// Possible entries for "ident". These are used in the "symbol", "value"
// and arginfo structures. Not every constant is valid for every use.
// In an argument list, the list is terminated with a "zero" ident; labels
// cannot be passed as function arguments, so the value 0 is overloaded.
enum IdentifierKind {
    iINVALID = 0,
    iVARIABLE = 1,      /* cell that has an address and that can be fetched directly (lvalue) */
    iARRAYELEM = 5,     /* array element, cell that must be fetched indirectly */
    iADDRESS = 6,       /* explicit address on the stack */
    iEXPRESSION = 7,    /* expression result, has no address (rvalue) */
    iCONSTEXPR = 8,     /* constant expression (or constant symbol) */
    iFUNCTN = 9,
    iACCESSOR = 13,     /* property accessor via a methodmap_method_t */
    iTYPENAME = 14,     /* symbol defining a type */
    iFIELD = 15,        /* field of a struct or enumstruct */
    iUPVAR = 16,        /* captured variable in closure */
};

enum class BuiltinType : uint8_t {
    Bool,
    Char,
    Int8,
    Int16,
    Int,
    Float,
    Double,
    Null,
    Any,
    Void,
    Int64,
    IntPtr,
};

enum class TypeKind : uint8_t {
    Builtin,
    Object,
    Function,
    EnumStruct,
    Class,
    Pstruct,
    Methodmap,
    Enum,
    Reference,
    Array,
    FunctionSignature,
    Typedef
};

struct funcenum_t;
class EnumStructDecl;
class ClassDecl;
class Expr;
class FunctionType;
class MethodmapDecl;
class PstructDecl;
class Type;

// Compact encoding of type + constness.
class QualType {
  public:
    QualType() : impl_(nullptr)
    {}
    QualType(Type* type) {
        impl_ = type;
    }
    explicit QualType(Type* type, bool is_const) {
        assert(type);
        impl_ = ke::SetPointerBits(type, is_const ? 1 : 0);
    }

    bool is_const() const {
        return ke::GetPointerBits<2>(impl_) == 1;
    }

    Type* operator *() const { return unqualified(); }
    Type* operator ->() const { return unqualified(); }
    Type* unqualified() const {
        return ke::ClearPointerBits<2>(impl_);
    }

    uint32_t hash() const { return ke::HashPointer(impl_); }

    QualType& operator =(Type* other) {
        impl_ = other;
        return *this;
    }

    bool operator ==(const QualType& other) const { return impl_ == other.impl_; }
    bool operator !=(const QualType& other) const { return impl_ != other.impl_; }
    explicit operator bool() const { return !!impl_; }

  private:
    Type* impl_;
};


struct TypenameInfo {
    static constexpr uintptr_t kAtomFlag = 0x1;
    static constexpr uintptr_t kLabelFlag = 0x2;

    TypenameInfo() {
        impl.raw = nullptr;
    }
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
        is_const(false),
        is_new(false),
        has_postdims(false),
        is_label(false),
        reference(false),
        resolved(false),
        resolved_array(false),
        is_varargs(false),
        is_auto(false)
    {}

    // Either null or an array of size |numdim|, pool-allocated.
    PoolArray<Expr*> dim_exprs;

    // Type information.
    Atom* type_atom;    // Parsed atom.
    Type* type;

    bool is_const : 1;
    bool is_new : 1;        // New-style declaration.
    bool has_postdims : 1;  // Dimensions, if present, were in postfix position.
    bool is_label : 1;      // If type_atom came from a tLABEL.
    bool reference : 1;
    bool resolved : 1;
    bool resolved_array : 1;
    bool is_varargs : 1;
    bool is_auto : 1;       // Type to be inferred from initializer.

    TypenameInfo ToTypenameInfo() const;

    bool bindable() const { return type_atom || type || is_auto; }

    void set_type(const TypenameInfo& rt) {
        if (rt.has_type()) {
            type_atom = nullptr;
            is_label = false;
            type = rt.type();
        } else {
            type_atom = rt.type_atom();
            is_label = rt.is_label();
            type = nullptr;
        }
    }

    void set_type(Type* t) { type = t; }

    QualType qualified() const { return QualType(type, is_const); }
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
    Type(Atom* name, TypeKind kind)
      : name_(name),
        kind_(kind),
        allowed_in_native_call_(true),
        inner_type_(nullptr)
    {}

    Atom* declName() const { return name_; }
    TypeKind kind() const { return kind_; }
    const char* kindName() const;
    const char* prettyName();

    template <class T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <class T> const T* as() const {
        if (T::is_a(this))
            return reinterpret_cast<const T*>(this);
        return nullptr;
    }

    template <class T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

    bool isBuiltin() const { return kind_ == TypeKind::Builtin; }
    bool isBuiltin(BuiltinType type) const { return isBuiltin() && builtin_type_ == type; }
    bool isInt() const { return isBuiltin(BuiltinType::Int); }
    bool isInt8() const { return isBuiltin(BuiltinType::Int8); }
    bool isInt16() const { return isBuiltin(BuiltinType::Int16); }
    bool isInt64() const { return isBuiltin(BuiltinType::Int64); }
    bool isIntPtr() const { return isBuiltin(BuiltinType::IntPtr); }
    bool isWideInt() const { return isInt64() || isIntPtr(); }
    bool isIntN() const { return isInt() || isInt64() || isIntPtr() || isInt16() || isInt8(); }
    bool isWideType() const { return isWideInt() || isDouble(); }
    bool isNull() const { return isBuiltin(BuiltinType::Null); }
    bool isChar() const { return isBuiltin(BuiltinType::Char); }
    bool isAny() const { return isBuiltin(BuiltinType::Any); }
    bool isVoid() const { return isBuiltin(BuiltinType::Void); }
    bool isFloat() const { return isBuiltin(BuiltinType::Float); }
    bool isDouble() const { return isBuiltin(BuiltinType::Double); }
    bool isReal() const { return isFloat() || isDouble(); }
    bool isBool() const { return isBuiltin(BuiltinType::Bool); }
    bool isReference() const { return kind_ == TypeKind::Reference; }
    bool isArray() const { return kind_ == TypeKind::Array; }
    bool isHeapItem();
    bool isTypedef() const { return kind_ == TypeKind::Typedef; }
    bool isCharArray() const;
    bool isNonHeapNullable() const;
    bool isFlatArray() const;
    bool isFixedArray() const;
    bool isNonFlatArray() const;
    bool isCompositeValue() const;
    bool isPassByRef() const;
    bool isAddressType() const;
    bool isNullable() const;

    bool isEnumOrMethodmap() const { return isEnum() || isMethodmap(); }

    // True if a value representation can be > 1 cell.
    bool isComposite() const { return isArray() || isEnumStruct(); }

    bool isAllowedInNativeCall() const { return allowed_in_native_call_; }
    void forbidInNativeCall() { allowed_in_native_call_ = false; }

    BuiltinType builtin_type() const {
        assert(isBuiltin());
        return builtin_type_;
    }

    // Size of an element in an array.
    std::optional<uint32_t> maybe_lit_size() const {
        if (isBuiltin()) {
            switch (builtin_type_) {
                case BuiltinType::Char:
                case BuiltinType::Int8:
                    return {1};
                case BuiltinType::Int16:
                    return {2};
                case BuiltinType::Bool:
                case BuiltinType::Int:
                case BuiltinType::IntPtr:
                case BuiltinType::Float:
                case BuiltinType::Null:
                case BuiltinType::Any:
                    return {4};
                case BuiltinType::Int64:
                case BuiltinType::Double:
                    return {8};
                default:
                    return {};
            }
        }
        switch (kind_) {
            case TypeKind::Methodmap:
            case TypeKind::Enum:
                return {4};
            default:
                return {};
        }
    }

    int podLoadSize() const {
        if (kind_ == TypeKind::Enum)
            return 4;
        if (kind_ != TypeKind::Builtin)
            return -1;
        switch (builtin_type_) {
            case BuiltinType::Char:
            case BuiltinType::Int8:
                return 1;
            case BuiltinType::Int16:
                return 2;
            case BuiltinType::Int:
            case BuiltinType::Float:
            case BuiltinType::Any:
            case BuiltinType::Bool:
                return 4;
            case BuiltinType::Double:
            case BuiltinType::Int64:
                return 8;
            default:
                return -1;
        }
    }

    uint32_t lit_size() const {
        return *maybe_lit_size();
    }

    Type* normalize() { return isReference() ? inner() : this; }

    bool coercesToInt() const {
        return coercesFromInt() || isAny();
    }

    bool coercesFromInt() const {
        if (kind_ == TypeKind::Enum || kind_ == TypeKind::Methodmap)
            return true;
        if (kind_ != TypeKind::Builtin)
            return false;
        switch (builtin_type_) {
            case BuiltinType::Bool:
            case BuiltinType::Char:
            case BuiltinType::Int8:
            case BuiltinType::Int16:
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
    bool isFunctionLike() const {
        return kind_ == TypeKind::Function || kind_ == TypeKind::FunctionSignature;
    }
    bool isCanonicalFunction() const {
        return isFunction() && !funcenum_ptr_;
    }
    bool isLegacyFunction() const;
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

    bool isClass() const {
        return kind_ == TypeKind::Object;
    }
    ClassDecl* asClass() const {
        if (!isClass())
            return nullptr;
        return class_ptr_;
    }

    bool isPstruct() const {
        return kind_ == TypeKind::Pstruct;
    }
    PstructDecl* asPstruct() const {
        if (!isPstruct())
            return nullptr;
        return pstruct_ptr_;
    }

    Type* inner() const {
        assert(isReference() || isArray() || isTypedef());
        return inner_type_;
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
    void setClass(ClassDecl* decl) {
        assert(kind_ == TypeKind::Object);
        class_ptr_ = decl;
        allowed_in_native_call_ = false;
    }
    void setPstruct(PstructDecl* decl) {
        assert(kind_ == TypeKind::Pstruct);
        pstruct_ptr_ = decl;
    }

    void setBuiltinType(BuiltinType type) {
        assert(kind_ == TypeKind::Builtin);
        builtin_type_ = type;
    }
    void setReference(Type* inner) {
        assert(!inner->isReference());
        assert(kind_ == TypeKind::Reference);
        inner_type_ = inner;
    }
    void setTypedef(Type* inner) {
        assert(kind_ == TypeKind::Typedef);
        assert(!inner->isTypedef());
        inner_type_ = inner;
    }

    void resetPtr();

  protected:
    Atom* name_;
    TypeKind kind_ : 8;
    bool allowed_in_native_call_ : 1;

    union {
        funcenum_t* funcenum_ptr_;
        MethodmapDecl* methodmap_ptr_;
        EnumStructDecl* enumstruct_ptr_;
        ClassDecl* class_ptr_;
        PstructDecl* pstruct_ptr_;
        BuiltinType builtin_type_;
        Type* inner_type_;
        QualType return_type_;
    };
};

class FunctionType : public Type {
  public:
    enum Convention {
        Legacy,
        Typed,
        Closure
    };

    FunctionType(QualType return_type, const std::vector<QualType>& args,
                 bool variadic, Convention conv)
      : Type(nullptr, TypeKind::FunctionSignature),
        conv_(conv),
        variadic_(variadic)
    {
        return_type_ = return_type;
        new (&args_) decltype(args_)(args);
        allowed_in_native_call_ = (conv == Legacy);
    }

    QualType return_type() const { return return_type_; }
    unsigned int nargs() const { return (unsigned int)args_.size(); }
    QualType arg_type(unsigned int i) { return args_[i]; }
    bool variadic() const { return variadic_; }
    Convention conv() const { return conv_; }
    bool needs_hidden_arg() const;

    static bool is_a(const Type* type) { return type->kind() == TypeKind::FunctionSignature; }

  private:
    PoolArray<QualType> args_;
    Convention conv_;
    bool variadic_;
};

class ArrayType : public Type {
  public:
    ArrayType(Type* inner, int size, bool is_flat);

    int size() const { return size_; }
    int rank() const { return rank_; }

    // Note that is_fixed() does not imply flat, but flat does imply fixed.
    // This is different from TypeDesc where Flat and Fixed are internally
    // separate types.
    bool is_fixed() const { return size_ != 0; }
    bool is_flat() const { return is_flat_; }

    static bool is_a(const Type* type) { return type->kind() == TypeKind::Array; }

  private:
    int size_;
    int rank_;
    bool is_flat_;
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
    Type* defineClass(Atom* name, ClassDecl* decl);
    Type* defineTag(Atom* atom);
    Type* definePstruct(PstructDecl* decl);
    Type* defineReference(Type* inner);
    Type* defineTypedef(Atom* name, Type* inner);
    Type* declareTypedef(Atom* name);
    void updateTypedef(Type* placeholder, Type* inner);
    ArrayType* defineArray(Type* element_type, int dim);
    ArrayType* defineArray(Type* element_type, const int* dim_vec, int numdim);
    ArrayType* defineArray(Type* element_type, const PoolArray<int>& dim_vec);
    ArrayType* defineFlatArray(Type* element_type, int dim);
    ArrayType* redefineArray(Type* element_type, ArrayType* old_type);
    FunctionType* defineFunction(QualType return_type,
                                 const std::vector<QualType>& args,
                                 bool variadic, FunctionType::Convention conv);
    FunctionType* UpdateReturnType(FunctionType* ft, QualType new_return_type);

    Type* type_object() const { return type_object_; }
    Type* type_null() const { return type_null_; }
    Type* type_function() const { return type_function_; }
    Type* type_any() const { return type_any_; }
    Type* type_void() const { return type_void_; }
    Type* type_float() const { return type_float_; }
    Type* type_double() const { return type_double_; }
    Type* type_bool() const { return type_bool_; }
    Type* type_string() const { return type_string_; }
    Type* type_char() const { return type_string_; }
    Type* type_int() const { return type_int_; }
    Type* type_int64() const { return type_int64_; }
    Type* type_intptr() const { return type_intptr_; }
    Type* type_int16() const { return type_int16_; }
    Type* type_int8() const { return type_int8_; }

    Type* GetBuiltin(BuiltinType type) const { return builtin_types_[(int)type]; }

  private:
    Type* add(const char* name, TypeKind kind);
    Type* add(Atom* name, TypeKind kind);
    void RegisterType(Type* type, bool unique_name = true);
    Type* defineBuiltin(const char* name, BuiltinType type);

  private:
    CompileContext& cc_;
    tr::unordered_map<Atom*, Type*> types_;
    tr::unordered_map<Type*, Type*> ref_types_;
    tr::vector<Type*> builtin_types_;
    std::vector<Type*> by_index_;
    Type* type_int_ = nullptr;
    Type* type_object_ = nullptr;
    Type* type_null_ = nullptr;
    Type* type_function_ = nullptr;
    Type* type_any_ = nullptr;
    Type* type_void_ = nullptr;
    Type* type_float_ = nullptr;
    Type* type_double_ = nullptr;
    Type* type_bool_ = nullptr;
    Type* type_string_ = nullptr;
    Type* type_int64_ = nullptr;
    Type* type_intptr_ = nullptr;
    Type* type_int16_ = nullptr;
    Type* type_int8_ = nullptr;

    struct ArrayCachePolicy {
        typedef ArrayType* Payload;

        struct Lookup {
            Type* type;
            int size;
            bool is_flat;
        };

        static bool matches(const Lookup& lookup, ArrayType* type);
        static uint32_t hash(const Lookup& lookup);
    };
    ke::HashTable<ArrayCachePolicy> array_cache_;

    struct FunctionCachePolicy {
        typedef FunctionType* Payload;

        struct Lookup {
            QualType return_type;
            const std::vector<QualType>* args;
            bool variadic;
            FunctionType::Convention conv;
        };

        static bool matches(const Lookup& lookup, FunctionType* type);
        static uint32_t hash(const Lookup& lookup);
    };
    ke::HashTable<FunctionCachePolicy> function_cache_;
};

static inline bool IsValueKind(IdentifierKind kind) {
    switch (kind) {
        case iINVALID:
        case iTYPENAME:
            return false;
        default:
            return true;
    }
}

} // namespace cc
} // namespace sp
