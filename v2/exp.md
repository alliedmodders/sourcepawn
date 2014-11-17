The compiler that appeared in Dr. Dobb's journal in 1984 is surprisingly similar to modern SourcePawn. To this day it relies on an ancient model of interleaving parsing with code generation. This design makes it extremely difficult to extend the language. The compiler also emits typeless code, which makes garbage collection all but impossible. The goal of this project - SourcePawn 2 - is to rewrite the compiler using modern techniques, and to introduce typed output.

SourcePawn 2 has some breaking changes over SourcePawn 1. In particular,
 - The name of a named enum does not create a constant with the enum size. I.e., "enum structs" are removed.
 - Braces on functions are now required.
 - Most compile-time directives have been removed or no longer have an effect. The following are still supported:
   - `#if`, `#else`, `#elseif`, `#endif`
   - `#define` for symbols only (macros are removed)
   - `#endinput`
   - `#pragma deprecated`
   - `#pragma newdecls`
 - It is no longer possible to require semicolons. Semicolons may still be used as normal. The relevant `#pragma` has no effect.
 - Some constructs are stricter about requiring newlines after braces, or semicolons on the same line.
 - `decl` is now the same as `new`. Arrays and variables are always filled with zeroes.
 - The operator overloading syntax for tags no longer exists.
