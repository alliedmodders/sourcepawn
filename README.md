SourcePawn
==========

A small and fast typed language for embedding in host applications.

Status
------

SourcePawn is an independent project from SourceMod, however SourceMod is its largest and possibly only consumer. Changes tend to be geared toward compatibility and efficiency with SourceMod and typical SourceMod programs. Their version numbers are currently synchronized.

There are two major development efforts in on/off progress:
 - "Transitional Syntax", which aims to add a basic C#-like type system to the language. The first iteration was released in SourcePawn 1.7, and is mostly a syntactic refactoring. The second iteration involves type safety in the API and runtime.
 - The "v2 compiler", which is an experimental C++ rewrite of the ancient C compiler. It currently has a working frontend, but no semantic analysis.

Overview
--------

SourcePawn requires the following dependencies:
 * [AMBuild](https://github.com/alliedmodders/ambuild)
 * [AMTL](https://github.com/alliedmodders/amtl)
 * A compiler with C++11 support. MSVC 2015+, GCC 4.8+, or Clang 3.0+ should work.

The SourcePawn source tree is divided into the following folders:
 - `compiler` - The legacy compiler, currently used in SourceMod.
 - `vm` - The virtual machine and just-in-time compiler.
 - `exp` - Experimental projects.
  - `compiler` - The v2 compiler for SourcePawn 1.7.
   - `docgen` - The documentation generator and web frontend.
   - `tools`
    - `docparse` - Parse files into JSON that can be consumed by documentation generators.

History
-------

SourcePawn has a long history - or rather, pre-history. It ultimately comes from SmallC, a language which appeared in Dr. Dobb's Journal in 1984.

Thiadmer Riemersma of ITB CompuPhase adopted this code - I suspect - to make a platform-independent language that could run on tiny embedded CPUs, such as 16-bit microcontrollers [1]. Thiadmer made sweeping changes to the language that made it less of a C dialect, so he renamed it to "Small". In particular, pointers were removed in favor of references, and the language became untyped. Each value in Small was a "cell". Cells could have tags, and operators could be defined for tags - but tags were not types. They could not change the storage width, they did not participate in a type system, and they were not type-checked.

Around 2000, Alfred Reynolds chose to use Small 1.8 as the scripting language for Admin-Mod, the first hugely popular Half-Life 1 server extension system. Admin-Mod had a lasting impact on the development scene for Valve-related games, and the choice to use Small extended to other systems as well. AMX Mod used Small 2.7, and when AlliedModders forked it for AMX Mod X, it took version 2.7 as well.

ITB CompuPhase renamed Small to "Pawn" in 2005 for better searchability. This renaming coincided with a 3.0 release. AMX Mod X upgraded to Pawn 3.0, which it still uses today.

### SourcePawn 1.0
In early 2006, Borja Ferrer ("faluco") and the author chose to re-use Pawn in SourceMod, but reimplemented the VM. Pawn had a crufty C API, and its JIT was written in self-modifying assembly. We introduced an object-oriented VM and replaced the JIT with a macro assembler written in C++. In the compiler frontend, AlliedModders made some notable changes:
 - A magic any tag was added which would accept any other tag.
 - A magic String tag was added which changed array storage to chars.
 - A decl keyword was introduced to skip zero-initialization of arrays.
 - Function types were added as magic tags, to allow passing functions by value.
 - Arrays could be initialized with sizes determined at run-time.
 - The semantics of division/modulus changed to match C's, for performance.
 - The type of a non-float cell was defined as a signed 32-bit integer, rather than a native integer.
 - The rational type was defined as a 32-bit IEEE-754 float.
 - Many language constructs were disabled - such as paren-less function calls, string packing sigils, and state machines.

Because these changes were fairly invasive, we renamed the language to "SourcePawn", and pinned the compiler to upstream version 3.2. We consider it an independent project and do not take upstream Pawn changes.

The new VM included a few important changes in design:
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

SourcePawn 1.7 was released with SourceMod 1.7 on February 4th, 2015.

The language has received a major (largely backwards compatible) overhaul called the ["Transitional Syntax"](https://wiki.alliedmods.net/SourcePawn_Transitional_Syntax "Transitional Syntax"). This introduces more C-like type declarations, more formal type checking, and the ability to simulate object-oriented behavior via "methodmaps". In addition, the compiler was ported to C++ for maintainability.

### SourcePawn 1.8.

SourcePawn 1.8 was released with SourceMod 1.8 on June 5th, 2016.

A few minor language bugs were fixed. Internally, the VM received a large overhaul to error-handling to improve stack frame dumps and future exception handling capabilities. The interpreter was removed.

### SourcePawn 1.9

SourcePawn 1.9 was declared stable on October 2nd, 2017.

It now includes a new reference interpreter, and will officially build and run on platforms other than x86.

### SourcePawn 1.10

SourcePawn 1.10 was declared stable on September 15th, 2019.

The biggest change in this release is an improved syntax for "enum structs" that works with Transitional Syntax. More information can be found [here](https://wiki.alliedmods.net/SourcePawn_Transitional_Syntax#Enum_Structs).

Other changes:
 - Removed `funcenum` and `functag` keywords.
 - Removed packed array indexing.
 - Improved compilation speed.
 - Added a control-flow verifier to the VM.

### SourcePawn 1.11

SourcePawn 1.11 is the current development version.
