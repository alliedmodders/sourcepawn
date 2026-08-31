Introducing SourcePawn 2.0.

Introducing SourcePawn 2.0.
===========================

SourcePawn can now allocate objects and arrays, and assign or return them like a
normal programming language.

The semantic path to get here was very narrow. We tried to maintain
compatibility as much as possible, which is very difficult in a language that
is full of weird low-level idiosyncracies.

The full scope of changes are divided into two sections: language changes, and
virtual machine changes (the implementation details).

Language Changes.
-----------------

### Arrays

Single-dimension fixed arrays are now referred to as "flat" arrays. Flat arrays
retain classic SourcePawn array semantics. They are stacked allocated,
copy-by-value, pass-by-reference, and return-by-value. Example:

    float vec_1[3] = {1.0, 2.0, 3.0};
    float vec_2[3] = vec1; // Deep-cop of vec_1.

All other array types are now heap-allocated. We refer to these as "heap"
arrays. Heap arrays can still be of fixed size. In this case, their size is
immutable, and is part of their type.

The following are examples of heap arrays:

    int[] n = new int[10];
    int bigarray[20][30];

Heap arrays are purely reference types. They can be returned or assigned
without any deep copying. For example, this is now legal:

    int[] MakeArray(int n) {
        return new int[n];
    }

They can also be assigned to globals:

    int[] gArray;

    void Init() {
        gArray = new int[MaxClients];
    }

And they can be assigned to inner arrays:

    int[][] gArray;

    void Init() {
        for (int i = 0; i < MaxClients; i++)
            gArray[i] = new int[10];
    }

Arrays now have an intrinsic "length" property:

    int sum(int[] array) {
        int x = 0;
        for (int i = 0; i < array.length; i++)
            x += array[i];
        return x;
    }

For compatibility reasons, there are some restrictions and idiosyncracies here.
Regardless of whether an array is stack or heap allocated, if it's assigned to
an array of fixed-size, it will result in a deep copy. For example:

    int gArray[10][20]; // All 2D arrays are heap allocated for compatibility.

    void Init() {
        int local[20];
        gArray[0] = local;      // deep copy.
        gArray[2] = gArray[3];  // also a deep copy
    }

Furthermore, it is not allowed for slices to escape. Slices are implicitly
created when converting from one array view to another. For example:

    void print(const char[] x) {}

    void f() {
        char x[255];
        print(x);

        char[] y = new char[255];
        print(y[2]);

    }

In both calls to `print`, a "slice array" is created that acts as a view into
the original array. Slices are restricted array objects that cannot appear on
the right-hand side of an escaping assignment. They can be passed to functions
but not assigned to globals, elements, or fields.

    char[] global;

    void DoStuff(char[] x) {
        global = x;
    }

This will compile, but will fail at runtime at the assign statement if called
like so:

    char local[255];
    DoStuff(local);

Since "local" is guaranteed to be stack allocated, if the slice were to be
assigned to a global variable, it would refer to invalid memory once the outer
function returns, creating an invalid use-after-free.

### Classes

SourcePawn now has support for classes. The syntax is almost identical to enum
structs, with a few changes and notes:

- Natives are not allowed, and objects may not flow to natives (including types
  that contain objects).
- The "public" keyword is optional. Class methods, fields, and properties are
  public by default.
- There is a "private" keyword to restrict access.
- Unlike enum structs, objects are not copied on assignment. They are reference
  types, and participate in garbage collection.
- Currently objects are reference counted. Cycles must be broken manually.
- There is no inheritance, so there is no "protected" keyword.

Example of using classes in SourcePawn:

    class Player {
        private int index_;
        private char[] name_;

        Player(int index) {
            index_ = index;

            char buffer[255];
            GetClientName(index_, buffer, sizeof(buffer));

            int size = strlen(buffer) + 1;
            name_ = new char[size];
            strcopy(name_, name_.size, buffer);
        }

        property int index {
            get() { return index_; }
        }
        property char[] name {
            get() { return name; }
        }
    }

    let player = new Player(client);
    PrintToServer("Player name: %s", player.name);

### Closures

SourcePawn now has support for nested functions, anonymous functions, and closures.
This feature was quite difficult to shim into the existing type system, so it comes
with some subtleties.

### Typed Signatures
By default, all functions now have what is referred to as a "typed" signature.
A typed signature can be declared with a new typedef syntax:

    typedef Callback = (int) -> Action;

Typed signatures can be called indirectly. For example:

    Action InvokeCallbacks(Callback[] callbacks, int client) {
        let result = Plugin_Continue;
        for (int i = 0; i < callbacks.size; i++) {
            let rv = callbacks[i](client);
            if (rv > result)
                result = rv;
        }
        return result;
    }

Functions using the "typed" signature style can be declared inside other
functions, either anonymously or not. Note that in this new syntax, the return
value comes after the argument list as an arrow. If omitted, the function
returns void.

    function void outer() {
        function inner1() -> int { return 5; }
        let inner2 = function () -> int { return 6; }
    }

#### Untyped Signatures

Meanwhile, legacy callbacks declared like this will have an "untyped" signature:

    typedef Callback = function Action ();

An untyped function can coerce to/from `any`, and can be passed to natives.
However, they cannot be invoked from within scripts. They can only be invoked
by natives. When casting from an untyped signature to a typed signature, a
run-time check occurs to ensure that underlying function's signature matches.

Functions, including non-nested functions, are now internally stored as
objects. They are implicitly casted to an untyped function ID in order to be
safely passed to natives. However, if a function has captured any local
variables, the implicit cast will fail at runtime (or compile-time, if
detected). This is because natives do not have access to the garbage collection
system, so it would be unsafe for a native to store an object with ephemeral
lifetime.

#### Classes

SourcePawn now has support for classes. A class describes a heap allocated
object, with syntax similar to enum structs. Unlike enum structs, an object is
always a pointer, and is assigned as a pointer. Like closures and arrays they
are garbage collected.

An example class:

    class Player {
        private int index_;

        Player(int index) {
            this.index_ = index;
        }

        void Print(const char[] text) {
            PrintToChat(index, text);
        }

        property int index {
            get() { return this.index; }
        }
    }

Class fields and methods are public by default, but can be made private with
a new "private" keyword. There is no inheritance, so there is no "protected"
keyword.

Unlike methodmaps, classes may not have any native functions. In addition,
values containing an object type may not be passed to natives, since natives
do not understand garbage collection.

Also unlike methodmaps, a class's constructor does not return the new object.
The new object is allocated internally as "this".

#### New Types

There are a number of new primitive types:

 - `intptr`: Equivalent to an `int` on 32-bit platforms, and an `int64` on
    64-bit platforms. Because the compiler does not know which platform the
    script will run on, `intptr` mostly behaves as an `int64`, with one
    exceptions. `int64` does not coerce to `intptr` as this could result in
    truncation. Similarly, `intptr` does not coerce to `int`.
 - `int16`: A 16-bit signed integer. These result in 2-byte storage when used
    in arrays. Internally, they are sign-extended to 32-bits when used in
    expressions. Thus wrap-around is allowed, with the exception that constants
    must fit in a 16-bit signed integer when directly assigned. When implicitly
    sign-extended, the result is truncated when stored back.
 - `int8`: An 8-bit signed integer. These result in 1-byte storage when used
    in arrays. Similar to `int16` they are sign extended and truncated as
    needed. `int8` is subtly different from `char` in two ways. First, it is
    sign-extended instead of zero-extended. Second, it is truncated on non-
    array assignment, whereas `char` is very inconsistently truncated.
 - `double`: A 64-bit IEEE-754 floating point number. The semantics are almost
    identical to `float`, with the exception that they have more precision and
    accuracy.

Implementation Changes
----------------------

### Garbage Collection

SourcePawn is now somewhat garbage collected. Heap-allocations are no longer
LIFO. Objects and arrays lifetimes are tracked using reference counting, and
are allocated using Microsoft's mimalloc library. This may seem an odd choice,
but the choice is very deliberate.

SourcePawn is designed for low-level game programming, and users have come to
expect performance guarantees for their code. Some hooks are fired hundreds of
times per frame, and with a ~15ms frame budget shared with the game itself, any
unexpected cost can cause frame skip.

As such, we chose a solution whereby the cost associated with object and array
allocation is _constant_. There is a small cost associated with allocating a
heap array, or an object, but it is constant. A huge clump of objects tied
to a single reference may be expensive to free, but the cost is constant. We
never kick into a garbage collection cycle, but we also don't have zero-cost
allocation.

There is no cycle collector, which means a self-referential object will leak
unless the cycle is explicitly broken.

We are very careful to internally hold all objects in Handles, an internal
abstraction which serves two functions. First, they ensure automatic Release
and AddRef calls. Second, it leaves open the door for a moving garbage
collector in some future timeline.

Because of the Handle abstraction, natives are not allowed access to objects
or array metadata. When passing a non-flat array to a native, the VM will
strip the metadata and only pass the interior data vector.

### New SMX

The virtual machine and opcode format have been completely rewritten. The SMX
bytecode stream is now stack-based, and is greatly simplified. In addition the
opcode stream relies heavily on RTTI, making it more type-safe, easy to verify,
easier to read, and easier to optimize. For example, here are the opcode streams
side by side for the same function from tests/int64/byref.sp:

```
+------------------------------------+------------------------------------+
|               SMX V2               |               SMX V1               |
+------------------------------------+------------------------------------+
| 0000: push.c.i64 1000              | 0000: proc                         |
| 0009: stor.s 0                     | 0004: break                        |
| 000c: addr.s 0                     | 0008: stor.s.c.i64                 |
| 000f: call test_ref                | 0018: addr.pri 1                   |
| 0014: load.s 0                     | 0020: stor.s.pri.i64               |
| 0017: stor.s 1                     | 0028: break                        |
| 001a: addr.s 1                     | 002c: addr.pri 0                   |
| 001d: call printnum64              | 0034: push.pri                     |
| 0022: retv                         | 0038: push.c 1                     |
|                                    | 0040: call test_ref                |
|                                    | 0048: break                        |
|                                    | 004c: addr.pri 0                   |
|                                    | 0054: addr.alt 1                   |
|                                    | 005c: move.i64                     |
|                                    | 0060: move.pri                     |
|                                    | 0064: push.pri                     |
|                                    | 0068: sysreq.n 1 printnum64        |
|                                    | 0074: break                        |
|                                    | 0078: zero.pri                     |
|                                    | 007c: retn                         |
|                                    | 0080: endproc                      |
+------------------------------------+------------------------------------+
```

Not only has the opcode count been reduced by a factor of two, but the opcodes
themselves are much smaller. The bytecode size is reduced from 132 bytes to 35
bytes in this example. As a consequence we've removed GZ compression from the
compiler.

You'll also note "break" opcodes are no longer emitted. The VM no longer relies
on them to find line boundaries.

### New Interpreter

There is a new, internal opcode format that is used for interpreting. This
format is designed for making the interpreter very fast. You can see an example
of the "lowered" bytecode with "smxdump --lower":

```
    0000: load.const.i64 1000, r0
    000c: addr.s r0, v4
    0012: call test_ref, 1, v4
    0021: move.i64 r0, r2
    0027: addr.s r2, v4
    002d: call printnum64, 1, v4
    003c: retv
```

The lowered bytecode is register-based and more aggressive in explicit typing.
It reduces the amount of high-level decoding the interpreter has to perform,
and also lowers the dispatch cost by removing stack operations. In this
example, we have to decide more opcode bytes, but dispatch less opcodes.

Just how much faster is this new interpreter? Between a 3-8X speedup compared
to the V1 interpreter, on average:

```
+---------------+--------------------+--------------------+
|     Test      | V2 x86-interp (ms) | V1 x86-interp (ms) |
+---------------+--------------------+--------------------+
| float-alu     |                165 |               1002 |
| int-alu       |                450 |               3108 |
| native-call   |                938 |               3855 |
| scripted-call |               2619 |               8970 |
+---------------+--------------------+--------------------+
```

The new just-in-time compilers for x86 and x64 have a few improvements as well.
A lot of redundant code has moved into the Environment, which makes the
compiler much simpler. In lines of code, they are about 15% smaller.

There are some other internal details that might be of interest. The stack now
grows up instead of down. Variadic arguments are now boxed into a temporary
array, meaning that outside of natives (for compatibility), there are no more
truly variadic functions.

A quirk of the new VM is that all scripted addresses must fit in a cell\_t, eg,
the VM will not work with addresses that are greater than 2GB. This is to make
the new array system work with old natives. LocalToPhysAddr() needs to
distinguish between a stack array (a bare address) with a heap array (a special
reference-counted object type). To do this, we tag the high bit of the cell.
The low bit would not work due to char arrays.

Furthermore, due to natives being hardcoded to take a cell\_t, and hardcoding
parameter indexes, the only performant solution available was to use pointer
compression.

On Win32, we use VirtualAlloc2 to make sure addresses are under 2GB. On Linux,
we use an mmap loop and some procmap parsing to find chunks in the first 2GB
of the virtual address space.

On 64-bit platforms, we reserve 2GB of virtual addresses up-front, and commit
it on demand.

This design, coupled with the fact that mimalloc does not support per-heap
virtual address space, means that all plugins share the same heap and stack.
The major benefit is that there is now zero-cost to move objects and arrays
between plugins. The downside is that unloading will henceforth be unreliable,
since plugin memory is no longer isolated.
