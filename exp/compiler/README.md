SourcePawn 2
============

The SourcePawn 2 compiler is a long-overdue complete rewrite of the SourcePawn language. It introduces new semantics, removes some old semantics, and opens the door for many new language possibilties. It is vastly more efficient and maintainable than the original compiler.

Status
------

Currently, preprocessing, parsing, name binding, and type resolution are implemented. Semantic analysis and code generation are a work-in-progress.

The current goal is to make the existing passes as solid as possible to use as a basis for other tools (such as syntax rewriting, better error reporting, and automatically generating documentation).

The goal is not to make this proto-compiler fully backward compatible with SourcePawn 1. However, when semantic analysis and code generation are implemented, alternatives to most missing language features will be provided.

Language Changes
----------------
SourcePawn 2 currently has the following language changes over SourcePawn 1:

 - Enum structs are removed. Enum values can no longer be tagged, and enum type names no longer create a named constant.
 - Braces are now required on functions.
 - Most compile-time (preprocessor) directives are removed. The following are allowed:
  - `#if`, `#else`, `#endif`. `#elseif` will most likely be supported as well.
  - `#define` is supported, although "macro functions" are not.
  - `#endinput` is supported.
  - `#pragma deprecated` is supported.
  - `#pragma newdecls` is supported.
  - `#pragma semicolon` is allowed, although it has no effect. Semicolons are always optional.
  - `#pragma dynamic` is allowed, although it may be ignored.
 - `decl` is now the same as `new`. Variables are always initialized to 0.
 - `functag` is removed in favor of `typedef`.
 - `funcenum` is removed in favor of `typeset`.
 - The operator overloading syntax for tags no longer exists.
 - All name binding now supports forward-binding.

Motivation
----------

The original SourcePawn 1 compiler is a single forward pipeline with no abstractions. Changing the language or code generation is extremely difficult. Our goal with the v2 compiler is to separate the compilation process into distinct phases, thus making it much easier to extend any given phase or component or introduce new tools or features.

Our goal is to maintain reasonable compatibility with SourcePawn 1, in the hopes that both the implementation path and adoption rate can be as painless as possible.

Design
------
A few structures are critical to bootstrapping the compiler, and these are worth describing since they are the heart of the implementation.

 - `StringPool` is responsible for *atomizing* strings. Two different string pointers with the exact same character sequence will be *atomized* to the same value (called an `Atom`). Atomization is critical for quickly computing string equivalence and simplifying string mapping. There is one `StringPool` for the entire compilation process.
 - `SourceManager` has two responsibilities. First, it caches the contents of files. Second, it tracks the *history* of tokens. For any token we parse out of a file, `SourceManager` can encode an id (called a `SourceLocation`) that can trace each macro it expanded from and each `#include` directive that caused the token to come into existence.
 - `ReportManager` is used for reporting messages (errors, warnings, and notes) encountered during the compilation process. Messages can contain other messages to assist the user in diagnosing a problem.
 - `PoolAllocator` is a bump-pointer memory allocator that does not require `free` or `delete`. It allocates large chunks ahead of time and freezes the chunks once compilation is finished. The compiler creates many, many tiny data structures in a rather complex web, so this ends up being much more efficient. PoolAllocators are generally needed for `CompileContext`s (below).
 - `CompileContext` is the entry point to the compilation process, and ties the above structures together. It holds command-line options, can automatically run compilation phases, and provides access to data structures across each phase. It is designed to be used for one invocation of the compiler pipeline.
 - `TypeManager` is where types are created, and it mainly serves as a cache for types that may be repeated frequently (such as `char[]` or `float[3]`).

In addition, the following phases are implemented:

 - `Preprocessor` is responsible for tokenizing an input file. In Pawn this is rather tricky since it has preprocessing directives which can expand into many new tokens, either from macros or from the `#include <file>` directive.  The `Preprocessor` handles all this complexity in a single forward pass, both to maintain proper token history and for performance. The `Preprocessor` may be considered as a stack of `Lexers`, which perform actual tokenization on a source or precomputed token stream.
 - `Parser` uses the `Preprocessor` to build an initial Abstract Syntax Tree (AST) from tokens in source files. This is where the actual grammar of the language is implemented. The parser is a simple recursive-descent parser and most of its complexity comes from handling new-decl versus old-decl syntax. This pass fails if the source file does not conform to SourcePawn grammar.

During parsing, two extra phases occur inline. These are name resolution, via `NameResolver`, and type resolution, via `TypeResolver`.
 - `NameResolver` attaches itself as a delegate to the Parser. The parser sends it notifications, and it uses these to build the scope hierarchy, symbol table, and perform all name binding. It also tries to resolve types if it can, since this alleviates pressure on the TypeResolver pass which is much more expensive. Any nodes that need complex type resolution, or constant resolution, are added to a work queue.
 - `TypeResolver` recursively walks nodes trying to resolve compile-time constants and types that are too complicated to resolve inline. For example, types that are used before they are defined, types that depend on constant expressions, or types that depend on expression inference. `TypeResolver` does not walk the entire AST; it relies on `NameResolver` giving it a list of nodes to operate on. This is important for reducing the size and complexity of the AST; over time, the `TypeResolver` and `NameResolver` will merge more closely.

Credits
-------

I owe a great deal to Clang for having excellent ideas for managing source streams, token history, and error reports. The v2 compiler's single-pipeline preprocessor-lexer, source manager, and error reporter are all heavily rooted in Clang's design.

Many other ideas are lessons learned (both the easy and hard ways) from the SpiderMonkey and v8 implementations of JavaScript.

The concept of "methodmaps" is based on a combination of C#'s partial classes and Golang's structural subtyping. Inspiration for the recursive type resolution algorithm (whether for better or worse) also came from Golang.

Inspiration for the design of the semantic analysis pipeline came from Clang. Clang, afforded (amazingly enough) by C++ not having implicit-forward-declaration, performs type checking as part of AST construction. While this doesn't quite work for SourcePawn, the concept has afforded us with many improvements.

SourcePawn 2 has been an extraordinary process of failed starts. Many, many rewrites (often with completely new semantics, such as dynamic typing) can be found scattered amongst AlliedModders, one dating as far back as 2009 called "iteration 3". Some of these are based on JavaScript, some C#, and oftentimes with implementation concepts from Lua. Many are source-only languages. Only time, much failure, and experience led to the current incarnation of very incremental improvements.

For the people who continue to use SourcePawn - and God bless them for putting up with the language - I'm grateful to you, for you have made this an interesting project.
