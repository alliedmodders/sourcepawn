SourcePawn
==========

A small and fast typed language for embedding in host applications.

Status
------

This repository is aimed at making SourcePawn independent from SourceMod. As of SourceMod 1.8, changes will be made to one and synced to the other.

The distribution of SourcePawn that currently ships with SourceMod is the `legacy` compiler. Other components in this repository have not shipped.

Changes to the legacy repository should be made against the SourceMod master until further notice. They will be synced here periodically.

Organization
------------

SourcePawn requires AMBuild to build.

The SourcePawn source tree is divided into the following folders:
 - `legacy` - The SourcePawn 1.7 compiler and VM.
 - `compiler` - The experimental SourcePawn 2.0 parser.
 - `tools`
  - `docparse` - Parse files into JSON that can be consumed by documentation generators.
 - `docgen` - The documentation generator and frontend tool.

History
-------

SourcePawn has a long history - or rather, pre-history. It ultimately comes from SmallC, a language which appeared in Dr. Dobb's Journal in 1984.

Thiadmer Riemersma of ITB CompuPhase adopted this code - I suspect - to make a platform-independent language that could run on tiny embedded CPUs, such as 16-bit microcontrollers [1]. Thiadmer made sweeping changes to the language that made it less of a C dialect, so he renamed it to "Small". In particular, pointers were removed in favor of references, and the language became untyped. Each value in Small was a "cell". Cells could have tags, and operators could be defined for tags - but tags were not types. They could not change the storage width, they did not participate in a type system, and they were not type-checked.

Around 2000, Alfred Reynolds chose to use Small 1.8 as the scripting language for Admin-Mod, the first hugely popular Half-Life 1 server extension system. Admin-Mod had a lasting impact on the development scene for Valve-related games, and the choice to use Small extended to other systems as well. AMX Mod used Small 2.7, and when AlliedModders forked it for AMX Mod X, it took version 2.7 as well.

ITB CompuPhase renamed Small to "Pawn" in 2005 for better searchability. This renaming coincided with a 3.0 release. AMX Mod X upgraded to Pawn 3.0, which it still uses today.

In early 2006, Borja Ferrer ("faluco") and I chose to re-use Pawn in SourceMod, but decided to reimplement the VM. Pawn had a crufty C API, and its JIT was written in self-modifying assembly. We introduced an object-oriented VM and replaced the JIT with a macro assembler written in C++. In the compiler frontend, AlliedModders made some notable changes:
 - A magic `any` tag was added which would accept any other tag.
 - A magic `String` tag was added which changed array storage to chars.
 - A `decl` keyword was introduced to skip zero-initialization of arrays.
 - Function types were added as magic tags, to allow passing functions by value.
 - The AMX file format was replaced with a new container format called SMX.
 - Arrays could be initialized with sizes determined at run-time.

Because these changes were fairly invasive, we renamed the language to "SourcePawn", and pinned the compiler to upstream version 3.2.

In 2014, after many failed attempts to write a new language, I introduced a "Transitional Syntax" to SourcePawn. This syntax introduces real types (such as `int` and `void`), a more C-like declaration style, and the ability to extend tags with methods as if they were objects.
