# Types

SourcePawn has the following type classes:

 - **Value Types** are enums, primitives, structs, and fixed-arrays. A
   variable or field of a value type cannot be `null`.
 - **Object Types** are non-fixed arrays and objects, allocated in the heap, and
   passed by-reference.
 - **Discriminate Types** are container types that can hold multiple types.
 - **Reference Types** are a pointer to a value, object, or dynamic type.
  
Every type has an *identity* for the purposes of determining type equivalence.
Two types are considered to be the same type if their identity is equal.

Grammar:

```
type-prefix ::= "const"? base-type
base-type ::=
    function-type
  | primitive-type
  | identifier
type-dims ::= ("[" "]")*
fixed-type-dims ::= ("[" integer "]")*
type-expr ::=
    type-prefix
  | type-prefix type-dims
  | type-prefix fixed-type-dims

typed-decl ::=
    type-prefix name
  | type-prefix type-dims name
  | type-prefix name fixed-type-dims
```

## Value Classes

In addition to type classes, SourcePawn defines two classes of values that can
be produced by an expression:

 - **l-values** are values that can appear on the left-hand side of an
   assignment.  These include index expressions (`a[x]`), field expressions
(`a.x`), and variable references. Even when constant, such expressions produce
an l-value`.
 - **cl-values** are l-values that are read-only. They can be used to compute
   immutable references, but they may not be used for assignment. cl-values
   are not transitively const.
 - **r-values** are values that are not l-values, and cannot appear on the
   left-hand side of an expression.

## Qualifiers

Certain types can be *qualified* in SourcePawn. There is one qualifier,
`const`, which changes the mutability of the type. `const` is transitive in
SourcePawn, and any value computed from a const type will result in a const-
qualified type.

When a primitive or enumeration declaration is const-qualified, it cannot be
changed via assignment, and must be initialized via a constant expression. The
compiler may eliminate references to the declaration by inlining the constant
value.

## Arrays

Arrays are conceptually represented as 3-tuples. They have the following components:
 - A *buffer*, specifying a contiguous block of memory at which values can be stored.
 - A *length*, specifying the number of slots in *buffer* that can be used.
 - A *capacity*, specifying the maximum size *length* can become before needing to reallocate *buffer*.
 
In SourcePawn 1, arrays are only an address, specifying a continguous block of memory. However, we specify different array types in terms of SP2 semantics anyway.
 
## Value Types

The following types are value types:

 - Enumeration values, produced by `enum`.
 - Struct values, produced by `struct`.
 - All primitive types.
 - Fixed-length arrays (such as `float[3]`).

### Enumerations

Named enumerations produce enum types. An enum type is identical to another
enum type if it was created by the same enumeration. Enum values are stored as
signed 32-bit integers, and cannot store values that are either larger 32-bit
integers or are not integers.

Grammar:

```
enum ::= "enum" enum-name? "{" enum-entry-list? "}" terminator
enum-name ::=
    label
  | name
enum-entry-list ::=
    enum-entry newline
  | enum-entry "," newline enum-entry-list
enum-entry ::= identifier ("=" expression)?
```

An enum entry that is initialized by an expression must be initialized by a constant expression.

### Methodmaps

A methodmap is an extension to an enum that allows it to be used as if it were a struct. Unlike a struct however, it cannot have fields, since it does not change the storage class of an enum. There can be at most one methodmap per enum.

Grammar:

```
methodmap ::= "methodmap" name "__nullable__"? "{" methodmap-body "}" terminator
methodmap-body ::= (methodmap-member newline)*
methodmap-member ::=
    record-method
  | record-property
```

### Structs

Structs are composite types produced by struct definitions. They are composed of zero or more named *fields* of any type, and each field is individually assignable by name.

When structs are assigned or passed by-value, their field values are copied in declaration order from the source struct to the destination struct, as if each field was individually assigned via the `=` operator.

A struct's storage is verified for cyclic references using the following algorithm. Cyclic structs are illegal.

 1. Given struct `S`:
 2. Let `Q` be the set of non-static fields with a struct type in `S`.
 3. While `Q` is non-empty,
   1. Remove a field `F` from `Q`.
   2. Let `S'` be the unqualified type of `F`.
   3. If `S = S'`, `S` is cyclic.
   4. Let `Q` be the union of `Q` and the set of non-static fields with a struct type in `S'`.
 4. `S` is not cyclic.

Grammar:

```
struct ::= "struct" name "{" struct-body "}" terminator
struct-body ::= (struct-member newline)*
struct-member ::=
    record-field
  | record-method
  | record-property

record-field ::= record-visibility typed-decl
record-property ::= type-expr identifier "{" property-body? "}"
property-body ::=
    property-setter? property-getter
  | property-getter? property-setter
property-getter ::=
    record-visibility "native" "get" "(" ")" terminator
  | record-visibility "get" "(" ")" "=" identifier newline
  | record-visibility "get" "(" ")" method-body newline
property-setter ::=
    record-visibility "native" "set" "(" typed-decl ")" terminator
  | record-visibility "set" "(" ")" "=" identifier newline
  | record-visibility "set" "(" typed-decl ")" method-body newline
record-method ::=
    record-visibility record-scope type-expr identifier "(" new-arg-list? ")" method-body newline
  | record-visibility record-scope "native" type-expr identifier "(" new-arg-list? ")" terminator
record-scope ::= "static"?
record-visibility ::= "public"

new-arg-list ::=
    new-arg
  | new-arg "," new-arg
new-arg ::= new-arg-base ('=' expression)?
new-arg-base ::=
    type-prefix "&"? identifier
  | type-prefix type-dims identifier
  | type-prefix identifier fixed-type-dims
```

An argument with a default value must be a constant expression. As an exception, an argument may contain a `sizeof` expression that computes the size of another argument.

### Primitives

Primitives are divided into sub-groups:

 - Integer types, either signed (`int8`, `int16`, `int32`, `int64`) or unsigned (`uint8`, `uint16`, `uint32`, `uint64`). The `int` type is an alias for `int32`.
 - Machine-width integer types, `intn` and `uintn`.
 - IEEE-754 Floating-point types (`float` and `double`).
 - Boolean (`bool`).
 - Character types, `char` (mostly equivalent to `int8`) and `codepoint` (an alias for `int32`).
 
Grammar:

```
primitive-type ::=
    bool
  | char
  | int8
  | uint8
  | int16
  | uint16
  | int32
  | uint32
  | int64
  | uint64
  | int
  | intn
  | uintn
  | float
  | double
  | any
```

Notes:

 - The `char` type is usually semantically equivalent to `int8`. 
 - The `any` primitive resolves to a special type called `unchecked`.
 - `int` is semantically equivalent to `int32`.
 - `intn` and `uintn` are not aliases - they are special types that do not support most implicit coercions. This is because they are intended to hold special values such as memory addresses, and they should be treated with care.

### Unchecked

`unchecked` is a type nearing deprecation that is used in place of generic/top types in SourcePawn. It is a 32-bit and essentially untyped, and has implicit coercion to enumartions, `int32`, and `float`.

When a memory location containing an `unchecked` is coerced to a specific type, or vice versa, the type is simply replaced and no conversion takes place. This can lead to unsafe conversions.

In the future, `unchecked` will be removed from the language.

### Fixed-Length Arrays
 
Fixed-length arrays are arrays that have a size as part of their type. This size must greater than or equal to zero. Two array types `T[M]` and `U[N]` are equivalent if `T` is equivalent to `U`, and `M = N`. Fixed-length arrays cannot be null. Fixed-length arrays always use the `fixed-type-dims` production (an exception exists for old-style declarations).

Fixed-length arrays are unusual in that their semantics are not strictly value-semantics. Fixed-length arrays *pass by reference* but *assign by value*. That is, when passing a fixed-length array to a parameter that accepts an equivalent type, its address is passed instead of its contents.

On the other hand, when assigning a fixed-length array to an equivalent type, each element of the source array is assigned to each corresponding element of the destination array, as if each had been assigned via the `=` operator.

In SourcePawn 2 semantics, a fixed-length array of size `N` can be thought of as a buffer with length `N` and capacity `N`. However, the compiler does not allow growing a fixed-length array.

## Object Types

Object types are allocated in the heap, and are passed and assigned by reference.

### Dynamic-Length Arrays

In SourcePawn 2, dynamic-length arrays are any array whose size is not known at compile-time (i.e., any non fixed-length arrays). They are passed by-reference. Two array types `T[]` and `U[]` are equivalent if `T` is equivalent to `U`. Dynamic-length arrays, like fixed-length arrays, cannot be null.

In SourcePawn 1, such arrays are referred to as "indeterminate" arrays. They are not truly dynamic as they cannot be resized. SourcePawn 1 does not support assigning to an indeterminate array.

### Functions

Functions are reference types that encode the signature, address, and environment of a function. SourcePawn 1 does not allow nested functions, so all of its function objects  have the global scope as their environment, and each is only instantiated once (as global code is only executed once).

In addition to function types, there exists a type called `Function`, also called the "metafunction" type. This is a deprecated type in SourcePawn 2, and is essentially the top-type of all functions.

Grammar:

```
function-type ::=
    "(" inner-function-type ")"
  | inner-function-type
inner-function-type ::= "function" type-expr "(" function-type-arg-list? ")"
function-type-arg-list ::=
    function-type-arg
  | function-type-arg (',' function-type-arg)
function-type-arg ::=
    type-expr-prefix "&"? identifier
  | type-expr-prefix type-dims identifier
  | type-expr-prefix identifier fixed-type-dims
```

## Discriminate Types

Discriminate types are containers that can hold exactly one value from a set of types. It is made safe by the fact that the container is *discriminated* - it knows the type of the value it holds.

### Typesets

Typesets are a set of types, and a value of a typeset must be one of the included types. Types in a typeset cannot be duplicated or ambiguous. That is, it is illegal to have two `int` fields in a typeset, or an `int` and a `const int` field.

Two typesets are equivalent if they were defined by the same typeset declaration.

Grammar:

```
typeset ::= "typeset" name "{" typeset-body? "}"
typeset-body ::= (typeset-entry newline)*
typeset-entry ::= type-expr
```

In SourcePawn 1.8 and earlier, a typeset may only contain function types.

## Reference Types

Non-array types can be passed indirectly, via a safe pointer called a *reference*. References can be a substitute for multiple return values, since they allow returning values through parameters. They can be thought of as one-element fixed-length arrays.

## Equivalence

`const` is ignored for equivalence for type entries in typesets, dynamic casts,
and in the types of function parameters, for the following value types:

 - All primitive types.
 - Enumeration types.
 - Function types.
 - The `unchecked` type.
 - The `Function` type.

This is called the "meaningless const" rule. The reasoning behind this is that
for these types, the `const` keyword only has meaning within the body of the
function.

When performing a dynamic cast from `S` to `const T`, if `T` is one of the
above types, the cast will be reduced to `T` and then coerced to `const`.

## Coercion

Implicit coercion is used for deciding how a type may be silently converted to another type. It is used during assignment, argument passing, and truth-testing. It should not be confused with casts (explicit coercions), operator promotions, or operator overloading, all of which are often built upon implicit coercion but are not quite the same.

SourcePawn defines the following coercion contexts:

 - *assignment*, using the `=` operator.
 - *argument*, either explicitly via a function or operator call, or implicitly via promotion for operators.
 - *shallow*, indicating that coercion should not be recursive.

The result of a coercion is one of the following. The first four coercions are considered errors.

 - *ambiguous* coercions occur if multiple coercions are possible.
 - *illegal* coercions occur if no coercion exists.
 - *const-discarding* coercions occur if a coercion would discard a const-qualifier.
 - *lossy* coercions indicate a possible coercion, but the coercion risks implicitly discarding data precision or length.
 - *conversion* coercions are legal coercions that perform some arbitrarily expensive operation to convert one type to another.
 - *trivial* coercions may either be no-operations (types are equivalent) or may simply need to add a const-qualifier.

If a trivial coercion does not result in a no-operation, it is still considered a coercion.

We define implicit coercion rules as follows, given value `V` of type `S`, to type `T`, as follows:

1. If `T` is a reference,
  1. If `V` is not an l-value or cl-value, the coercion is illegal.
  2. If `S` is a reference type, let `S'` be the inner type of `S&`.
  3. Otherwise, let `S'` be `S`.
  4. Let `T'` be the inner type of `T&`.
  5. If `V` is a cl-value,
    1. If `T` is const-qualified, proceed with step 1.6.
    2. If `T'` is not const-qualified, the coercion is illegal.
    3. If `T'` does not have meaningless const coercion, the coercion is illegal.
  6. If `S'` is const-qualified, and `T'` is not const-qualified, the coercion is illegal.
  7. If unqualified `S'` is not equivalent to unqualified `T'`,
    1. If `T'` is not `unchecked`, the coercion is illegal.
    2. If `S'` is not `int32`, `float`, or an enumeration, the coercion is illegal.
    3. Let `V` be viewed as a `T'` and proceed with step 1.8.
  8. `V` has trivial coercion to `T'`.
  9. Return a reference `T&` to `V`; this does not count as a cast.
2. If `S` is a reference,
  1. Let `S` be the inner type of the reference `S&`, with the same const-qualifiers.
  2. Let `V` be the result of dereferencing `V`.
  3. Proceed with coercion; the dereference does not count as a cast or coercion.
3. If `T` is a struct,
  1. If unqualified `S` is not equivalent to unqualified `T`, the coercion is illegal.
  2. If `S` is const-qualified and `T` is not const-qualified, the coercion is illegal.
  3. `V` has trivial coercion to `T`.
4. If `T` is an enumeration,
  1. If `T` has a nullable methodmap, and `S` is `null_t`, the coercion is trivial.
  2. If `S` is `unchecked`, and the coercion is for assignment, a new `T` is constructed using the exact bit pattern in `S`.
  3. If unqualified `S` is not equivalent to unqualified `T`, the coercion is illegal.
  4. `V` does not require coercion to `T`.
5. If `T` is `unchecked`,
  1. If `S` is `unchecked`, `V` does not require coercion to `T`.
  1. If `S` is not a primitive type or enumeration, the coercion is illegal.
  2. If `S` is an enumeration, its signed integer is returned.
  3. If `S` is `double`, `int64`, `uint64`, `intn`, or `uintn`, the coercion is illegal.
  4. If `S` is `float`, the `V` is converted to a signed 32-bit integer containing the IEEE-754 bit encoding of the `float` value.
  5. If `S` is a signed integer, it is sign-extended to an `int32` and returned.
  6. If `S` is an unsigned integer, it is sign-extended to a `uint32` and converted to a signed 32-bit integer containing the twos-complement encoding of the `int32` value.
  7. If `S` is a bool, it is zero-extended to an `int32` and returned.
6. If `T` is `bool`,
  1. If `S` is an integer, `unchecked`, or enumeration value, it is converted to `true` if non-zero and `false` if zero.
  2. If `S` is `float` or `double`, it is converted to `false` if `NaN` or zero, and `true` otherwise.
  3. If `S` is `bool`, `V` does not require coercion to `T`.
  4. Otherwise, the coercion is illegal.
7. If `T` is a `double` or `float`,
  1. If `S` is not a primitive, the coercion is illegal.
  2. If `S` is `intn` or `uintn`, the coercion is illegal.
  3. If `S` is `float`, and `T` is `double`, `V` is converted to a `double`.
  4. If `S` is `double` and `T` is `float`, the coercion fails as lossy.
  5. If `S` is an integer, it is converted to `T`.
  6. If `S` is equivalent to `T`, `V` does not need coercion to `T`.
  7. Otherwise, the coercion is illegal.
8. If `T` is an integer or `char`,
  1. If `S` is neither a primitive nor `unchecked`, the coercion is illegal.
  2. If `S` is `unchecked`, and `T` is not `int32` or `uint32`, the coercion is illegal.
  3. If `S` is a `float` or `double`, coercion fails as lossy.
  4. If `S` is a `bool`, the coercion is illegal.
  5. If `T` is an `intn` or `uintn`, and `S` is not equivalent to `T`, the coercion is illegal.
  6. If `S` has a larger bit width than `T`, the coercion is illegal.
  7. If `S` has a smaller bit width than `T`, given `S`'s bit-width as `m` and `T`'s bit-width as `n`:
     1. If `S` is unsigned, it is zero-extended to an unsigned integer of `n` bits.
     2. If `S` is signed, it is sign-extended to a signed integer of `n` bits.
  8. If `S` is signed and `T` is unsigned, `S` is converted to a `T` containing the same bits.
  9. If `S` is unsigned and `T` is signed, `S` is converted to a `T` containing the same bits.
  10. Otherwise, `V` does not require coercion to `T`.
9. If `T` is a function,
  1. If `S` is `null_t`, `null` is returned as a `T`.
  2. If `S` is not a function, the coercion is illegal.
  3. If the return types of `T` and `S` are not equivalent, the coercion is illegal.
  4. If the parameter counts of `T` and `S` are not equivalent, the coercion is illegal.
  5. For each parameter index `i` in `T`, if the parameter type at index `i` in `T` is not equivalent to the parameter type at index `i` in `S`, the coercion is illegal.
  6. `V` does not require coercion to `T`.
10. If `T` is a typeset,
  1. If unqualified `S` is equivalent to unqualified `T`,
    1. If `S` is const-qualified and `T` is not const-qualified, the coercion is illegal.
    2. Otherwise, `V` has trivial coercion to `T`.
  2. If this is a shallow coercion, the coercion is illegal.
  3. If `S` has a type in its typeset equivalent to `T`,
    1. Let `V'` be a new `T`, discriminated as an `S`, with value `V`.
    2. `V'` has trivial coercion to `T`.
  4. If `S` has shallow coercion to more than one field in `T`, the coercion is ambiguous.
  5. If `S` has no coercion to any field in `T`, the coercion is illegal.
  6. Let `T'` be the matched coerceable type in `T`.
  7. Let `V'` be the result of shallow coercing `V` to `T'`.
  8. Let `V''` be a new typeset of type `T`, discriminated as `T'`, with value `V'`.
  9. `V''` has trivial coercion to `T`.
11. If `T` is a fixed-length array,
  1. If `S` is not a fixed-length array, the coercion is illegal.
  2. If `S` is const-qualified and `T` is not const-qualified, the coercion is illegal.
  3. Let `S'` be the type contained by array `S`.
  4. Let `T'` be the type contained by array `T`.
  5. If `S'` is not equivalent to `T'`, the coercion is illegal.
  6. Let `Sn` be the fixed-length of `S`.
  7. Let `Tn` be the fixed-length of `T`.
  8. If `Sn` is equal to `Tn`, `V` has trivial coercion to `T`.
  9. If the coercion is for assignment, and `S'` is `char`,
    1. If `Sn < Tn`, return a new fixed-length array with a buffer of length `Tn`, and length and capacity `Tn`. The first `Sn` elements of the buffer should be equal to the first `Sn` elements of `V`'s buffer, and the remaining elements should contain 0.
  10. Otherwise, the coercion is illegal.
12. If `T` is an indeterminate array,
  1. If `S` is not an array, the coercion is illegal.
  2. If `S` is const-qualified and `T` is not const-qualified, the coercion is illegal.
  3. Let `S'` be the type contained by array `S`.
  4. Let `T'` be the type contained by array `T`.
  5. If `S'` is not equivalent to `T'`, the coercion is illegal.
  6. If `S` is fixed-length,
    1. Let `Sn` be the fixed-length of `S`.
    2. Let `V'` be a new array of `T'` with the buffer of `V`, length `Sn`, and capacity `Sn`.
    3. `V'` has trivial coercion to `T`.
  7. `V` has trivial coercion to `T`.
13. If `T` is `Function`,
  1. If `S` is a function, `V` is returned as a `Function`.
  2. If `S` is `null_t`, `V` is returned as a `Function`.
  3. If `S` is `Function`, `V` does not require coercion to `T`.
  4. Otherwise, the coercion is illegal.
14. If `T` is `null_t`,
  1. If `S` is `null_t`, `V` has trivial coercion to `T`.
  2. Otherwise, the coercion is illegal.
