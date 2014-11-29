SourcePawn 1
------------

This is the legacy version of SourcePawn, iniitally based on Pawn 3.2 from ITB CompuPhase. It has two components:
 - `compiler` - The compiler which generates .smx files from source input.
 - `vm` - The virtual machine and just-in-time compiler.

History
=======


### SourcePawn 1.0
SourcePawn 1.0 was forked from Pawn 3.2 in early 2006. The just-in-time compiler was co-authored by David Anderson and Borja Ferrer as a clean-room, C++ implementation of the VM, to reduce dependence on difficult areas of the original AMX runtime.

The following language changes were introduced:

 - A magic any tag was added which would accept any other tag.
 - A magic String tag was added which changed array storage to chars.
 - A decl keyword was introduced to skip zero-initialization of arrays.
 - Function types were added as magic tags, to allow passing functions by value.
 - Arrays could be initialized with sizes determined at run-time.
 - The semantics of division/modulus changed to match C's, for performance.
 - The type of a non-float cell was defined as a signed 32-bit integer, rather than a native integer.
 - The rational type was defined as a 32-bit IEEE-754 float.
 - Many language constructs were disabled - such as paren-less function calls, string packing sigils, and state machines are examples.

A few important changes were made in the VM.

 - The API became object oriented.
 - The only execution mode was a whole-program just-in-time compiler for x86. The JIT used a two-pass macro assembler; the first pass computed jump targets and instruction sizes.
 - The AMX file format was replaced with a new container format called SMX. The in-memory AMX format was translated to SMX as part of the output process.

The initial implementation of the VM API was embedded directly in SourceMod, and the JIT was a separate closed-source binary.

Additionally, a problem was anticipated from AMX Mod X: many values backed by C++ data structures were opaque and therefore not well typed. As an attempt to mitigate this, a "Handle" system was introduced similar to file or kernel objects from operating systems. It was a tuple of 16-bit values, a serial number and an index into a Handle table. Each slot in the Handle table was typed (types were registered by participating API) and objects participating in the Handle API were internally reference counted.

The Handle system was not part of the VM, and although it provided many benefits over precursor systems, this proved to have severe impact on the future ability to make Pawn garbage-collection-safe. The bare API provided no way to safely communicate the lifetime of values in the VM.

### SourcePawn 1.1

SourcePawn 1.1 was released with SourceMod 1.1, on December 28th, 2008.

This was a major refactoring of SourcePawn internals:
 - The virtual machine API and the JIT were unified into the same library.
 - The JIT became more incremental, compiling functions on-demand rather than all functions at once.
 - Debug mode was removed in favor of a low-overhead, always-on mechanism for recovering stack traces.
 - The C++ API was overhauled to deprecate remnants of the original AMX API design.

There were no language changes.

For the SourceMod 1.4 release (October 28, 2011), many O(n^2) algorithms were removed from the SourcePawn compiler to improve performance.

### SourcePawn 1.2

SourcePawn 1.2 was released with SourceMod 1.6, on July 3rd, 2013.

A string literal concatenation operator was pulled from upstream Pawn.

The virtual machine received many changes:
 - The JIT was rewritten updated to use a much simpler macro assembler that operates in a single forward pass.
 - An experimental interpreter was added (the JIT is still considered the reference implementation).
 - A off-thread watchdog timer for infinite loops was added.

### SourcePawn 1.7.

SourcePawn 1.7 is in development as part of the SourceMod 1.7 development branch.

The language has received a major (largely backwards compatible) overhaul called the ["Transitional Syntax"](https://wiki.alliedmods.net/SourcePawn_Transitional_Syntax "Transitional Syntax"). This introduces more C-like type declarations, more formal type checking, and the ability to simulate object-oriented behavior via "methodmaps". In addition, the compiler was ported to C++ for maintainability.
