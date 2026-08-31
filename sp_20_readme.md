Introducing SourcePawn 2.0.

Introducing SourcePawn 2.0.
===========================

SourcePawn can now allocate objects and arrays, and assign or return them like a
normal programming language.

The path to get here was very narrow. We tried to maintain compatibility as much
as possible, which is very difficult in a language that is full of weird
low-level idiosyncracies.

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

All other array types are now heap-allocated. We refer to these as "dynamic"
arrays. Within the space of dynamic arrays, there are still fixed arrays. The
only difference is in type-checking and mutability of the array size.

The following are examples of dynamic arrays:

    int[] n = new int[10];
    int bigarray[20][30];

Dynamic arrays are purely reference types. They can be returned or assigned
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
First, a flat array can be assigned to a dynamic array, but only of fixed size.
The following is an example that would perform a deep copy:

    int gArray[10][20]; // All 2D arrays are heap allocated for compatibility.

    void Init() {
        int local[20];
        gArray[0] = local; // deep copy.
    }

Second, it is not allowed for slices to escape. Slices are implicitly created
when converting from one array view to another. For example:

    void print(const char[] x) {}

    void f() {
        char x[255];
        print(x);

        char[] y = new char[255];
        print(y[2]);

    }

In both calls to `print`, a "slice array" is created that acts as a view into
the original array. Slices are restricted array objects that cannot appear on
the right-hand side of an escaping assignment. They can be passed or returned in
functions, but not assigned to globals, elements, or fields.

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

Implementation Changes
----------------------

### Garbage Collection

SourcePawn is now somewhat garbage collected. Heap-allocations are no longer
LIFO. Objects and arrays lifetimes are tracked using reference counting, and
are allocated using Microsoft's mimalloc library. This may seem an odd choice,
but the choice is very deliberate.

SourcePawn is designed for server-side low-level programming, and users have
come to expect performance guarantees for their code. Some hooks are fired
hundreds of times per frame, and with a ~15ms frame budget shared with the game
itself, any unexpected cost can cause frame skip.

As such, we chose a solution whereby the cost associated with object and array
allocation is _constant_. There is a small cost associated with allocating a
dynamic array, or an object, but it is constant. A huge clump of objects tied
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
|               SMX V1               |               SMX V2               |
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
