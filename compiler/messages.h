/*  Pawn compiler - Error message strings (plain and compressed formats)
 *
 *  Copyright (c) ITB CompuPhase, 2000-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */

// To find unused errors, try this:
//   for i in {1..182}; do echo -n "Error $i:" ; grep -E "\($i(,|\))" compiler/sc*.cpp | wc -l; done

static const char* errmsg[] = {
    /*001*/ "expected token: \"%s\", but found \"%s\"\n",
    /*002*/ "only a single statement (or expression) can follow each \"case\"\n",
    /*003*/ "declaration of a local variable must appear in a compound block\n",
    /*004*/ "function \"%s\" is not implemented\n",
    /*005*/ "function may not have arguments\n",
    /*006*/ "must be assigned to an array\n",
    /*007*/ "operator cannot be redefined\n",
    /*008*/ "must be a constant expression; assumed zero\n",
    /*009*/ "invalid array size (negative, zero or out of bounds)\n",
    /*010*/ "invalid function or declaration\n",
    /*011*/ "symbol '%s' already defined - did you use a methodmap before its declaration?\n",
    /*012*/ "invalid function call, not a valid address\n",
    /*013*/ "no entry point (no public functions)\n",
    /*014*/ "invalid statement; not in switch\n",
    /*015*/ "\"default\" case must be the last case in switch statement\n",
    /*016*/ "multiple defaults in \"switch\"\n",
    /*017*/ "undefined symbol \"%s\"\n",
    /*018*/ "initialization data exceeds declared size\n",
    /*019*/ "not a label: \"%s\"\n",
    /*020*/ "invalid symbol name \"%s\"\n",
    /*021*/ "symbol already defined: \"%s\"\n",
    /*022*/ "must be lvalue (non-constant)\n",
    /*023*/ "array assignment must be simple assignment\n",
    /*024*/ "\"break\" or \"continue\" is out of context\n",
    /*025*/ "function heading differs from prototype\n",
    /*026*/ "no matching \"#if...\"\n",
    /*027*/ "invalid character constant\n",
    /*028*/ "invalid subscript (not an array or too many subscripts): \"%s\"\n",
    /*029*/ "invalid expression, assumed zero\n",
    /*030*/ "compound statement not closed at the end of file (started at line %d)\n",
    /*031*/ "unknown directive\n",
    /*032*/ "array index out of bounds (variable \"%s\")\n",
    /*033*/ "array must be indexed (variable \"%s\")\n",
    /*034*/ "argument does not have a default value (argument %d)\n",
    /*035*/ "argument type mismatch (argument %d)\n",
    /*036*/ "empty statement\n",
    /*037*/ "invalid string (possibly non-terminated string)\n",
    /*038*/ "extra characters on line\n",
    /*039*/ "constant symbol has no size\n",
    /*040*/ "duplicate \"case\" label (value %d)\n",
    /*041*/ "invalid ellipsis, array size is not known\n",
    /*042*/ "invalid combination of class specifiers\n",
    /*043*/ "character constant exceeds range for packed string\n",
    /*044*/ "positional parameters must precede all named parameters\n",
    /*045*/ "too many function arguments\n",
    /*046*/ "unknown array size (variable \"%s\")\n",
    /*047*/ "array sizes do not match, or destination array is too small\n",
    /*048*/ "array (s do not match\n",
    /*049*/ "invalid line continuation\n",
    /*050*/ "constant '%s' already defined\n",
    /*051*/ "invalid subscript, use \"[ ]\" operators on major dimensions\n",
    /*052*/ "multi-dimensional arrays must be fully initialized\n",
    /*053*/ "exceeding maximum number of dimensions\n",
    /*054*/ "unmatched closing brace (\"}\")\n",
    /*055*/ "start of function body without function header\n",
    /*056*/ "arrays, local variables and function arguments cannot be public (variable \"%s\")\n",
    /*057*/ "unfinished expression before compiler directive\n",
    /*058*/ "duplicate argument; same argument is passed twice\n",
    /*059*/ "function argument may not have a default value (variable \"%s\")\n",
    /*060*/ "multiple \"#else\" directives between \"#if ... #endif\"\n",
    /*061*/ "\"#elseif\" directive follows an \"#else\" directive\n",
    /*062*/ "number of operands does not fit the operator\n",
    /*063*/ "function result tag of operator \"%s\" must be \"%s\"\n",
    /*064*/ "cannot change predefined operators\n",
    /*065*/ "enum struct fields cannot have more than one dimension\n",
    /*066*/ "function argument may not be a reference argument or an array (argument \"%s\")\n",
    /*067*/ "variable cannot be both a reference and an array (variable \"%s\")\n",
    /*068*/ "invalid rational number precision in #pragma\n",
    /*069*/ "rational number format already defined\n",
    /*070*/ "rational number support was not enabled\n",
    /*071*/ "user-defined operator must be declared before use (function \"%s\")\n",
    /*072*/ "\"sizeof\" operator is invalid on \"function\" symbols\n",
    /*073*/ "function argument must be an array (argument \"%s\")\n",
    /*074*/ "#define pattern must start with an alphabetic character\n",
    /*075*/ "input line too long (after substitutions)\n",
    /*076*/ "syntax error in the expression, or invalid function call\n",
    /*077*/ "malformed UTF-8 encoding, or corrupted file: %s\n",
    /*078*/ "function uses both \"return\" and \"return <value>\"\n",
    /*079*/ "inconsistent return types (array & non-array)\n",
    /*080*/ "unknown symbol, or not a constant symbol (symbol \"%s\")\n",
    /*081*/ "enum struct field arrays must have fixed sizes\n",
    /*082*/ "properties cannot be arrays\n",
    /*083*/ "methodmap methods cannot return arrays\n",
    /*084*/ "cannot call constructor for '%s' as static method\n",
    /*085*/ "type \"%s\" can only be used in new-style declarations\n",
    /*086*/ "using enum struct \"%s\" exceeds maximum dimension count\n",
    /*087*/ "enum struct \"%s\" cannot refer to itself\n",
    /*088*/ "cannot return a value from a void function\n",
    /*089*/ "casting a void function is illegal\n",
    /*090*/ "public functions may not return arrays (symbol \"%s\")\n",
    /*091*/ "ambiguous constant; tag override is required (symbol \"%s\")\n",
    /*092*/ "number of arguments does not match definition\n",
    /*093*/ "expected tag name identifier\n",
    /*094*/ "cannot assign const qualifier to enum struct field \"%s\"\n",
    /*095*/ "type \"%s\" cannot be applied as a tag\n",
    /*096*/ "could not find member \"%s\" in struct \"%s\"\n",
    /*097*/ "symbol \"%s\" does not have a matching type\n",
    /*098*/ "type \"%s\" should be \"%s\" in new-style declarations\n",
    /*099*/ "%s should not have an explicit return type\n",
    /*100*/ "function prototypes do not match\n",
    /*101*/ "specify either all dimensions or only the last dimension\n",
    /*102*/ "cannot find %s %s\n",
    /*103*/ "%s was already defined on this %s\n",
    /*104*/ "cannot find any methods for %s\n",
    /*105*/ "cannot find method or property %s.%s\n",
    /*106*/ "cannot call methods on an array\n",
    /*107*/ "cannot call methods on a function\n",
    /*108*/ "resolution operator (::) can only resolve field offsets of enum structs\n",
    /*109*/ "%s name must start with an uppercase letter\n",
    /*110*/ "%s has already been defined (previously seen as %s)\n",
    /*111*/ "cannot index into enum struct \"%s\"\n",
    /*112*/ "resolution operator (::) cannot be used on \"%s\"\n",
    /*113*/ "constructor for \"%s\" already exists\n",
    /*114*/ "missing type, or %s must have the same name as %s \"%s\"\n",
    /*115*/ "cannot use delete, %s %s has no destructor\n",
    /*116*/ "no methodmap or class was found for %s\n",
    /*117*/ "enum structs cannot be indexed as arrays\n",
    /*118*/ "custom destructors are no longer supported\n",
    /*119*/ "enum struct \"%s\" must have at least one field\n",
    /*120*/ "methodmap and class signatures must use new-style type declarations\n",
    /*121*/ "cannot specify array dimensions on both type and name\n",
    /*122*/ "expected type expression\n",
    /*123*/ "fully-qualified name \"%s\" is too long, would be truncated to \"%s\"\n",
    /*124*/ "unexpected token, expected method or property\n",
    /*125*/ "expected \"native\", \"get\", or \"set\"\n",
    /*126*/ "%s for %s already exists\n",
    /*127*/ "property getters cannot accept extra arguments\n",
    /*128*/ "cannot return an array of indeterminate length\n",
    /*129*/ "cannot mix methodmaps and classes with inheritance\n",
    /*130*/ "cannot coerce functions to values\n",
    /*131*/ "cannot coerce object type %s to non-object type %s\n",
    /*132*/ "cannot coerce non-object type %s to object type %s\n",
    /*133*/ "cannot coerce unrelated object types %s and %s\n",
    /*134*/ "type mismatch (%s and %s)\n",
    /*135*/ "cannot use enum struct type \"%s\" in natives\n",
    /*136*/ "reference is redundant, enum struct types are array-like\n",
    /*137*/ "cannot mix reference and array types\n",
    /*138*/ "const was specified twice\n",
    /*139*/ "could not find type \"%s\"\n",
    /*140*/ "new-style array types cannot specify dimension sizes as part of their type\n",
    /*141*/ "natives, forwards, and public functions cannot return arrays\n",
    /*142*/ "unexpected array expression\n",
    /*143*/ "new-style declarations should not have \"new\"\n",
    /*144*/ "void cannot be used as a variable type\n",
    /*145*/ "invalid type expression\n",
    /*146*/ "#pragma newdecls must be required or optional\n",
    /*147*/ "new-style declarations are required\n",
    /*148*/ "cannot assign null to a non-nullable type\n",
    /*149*/ "no getter found for property %s\n",
    /*150*/ "setter must take exactly one extra argument with type %s\n",
    /*151*/ "unmatched opening brace ('{') (line %d)\n",
    /*152*/ "no setter found for property %s\n",
    /*153*/ "unused153\n",
    /*154*/ "cannot assign INVALID_FUNCTION to a non-function type\n",
    /*155*/ "expected newline, but found '%s'\n",
    /*156*/ "invalid 'using' declaration\n",
    /*157*/ "'%s' is a reserved keyword\n",
    /*158*/ "multi-tags are no longer supported\n",
    /*159*/
    "brackets after variable name indicate a fixed-size array, but size could not be determined - "
    "either specify sizes, an array initializer, or use dynamic syntax (such as 'char[] x')\n",
    /*160*/
    "brackets in between type and variable name indicate a dynamic-size array, but a fixed-size "
    "initializer was given\n",
    /*161*/
    "brackets after variable name indicate a fixed-size array, but a dynamic size was given - did "
    "you mean to use 'new %s[size]' syntax?\n",
    /*162*/
    "cannot create dynamic arrays in global scope - did you mean to create a fixed-length array "
    "with brackets after the variable name?\n",
    /*163*/ "indeterminate array size in \"sizeof\" expression (symbol \"%s\")\n",
    /*164*/ "allocated array type '%s' doesn't match original type '%s'\n",
    /*165*/
    "cannot create dynamic arrays in static scope - did you mean to create a fixed-length array "
    "with brackets after the variable name?\n",
    /*166*/ "cannot use 'this' outside of a methodmap method or property\n",
    /*167*/ "cannot use delete, %s do not have destructors\n",
    /*168*/ "re-tagging enums is no longer supported\n",
    /*169*/ "cannot tag an enum as implicit-int\n",
    /*170*/ "creating new object '%s' requires using 'new' before its constructor\n",
    /*171*/ "cannot use 'new' with non-object-like methodmap '%s'\n",
    /*172*/ "methodmap '%s' does not have a constructor\n",
    /*173*/
    "'%s' is a newly reserved keyword that may be used in the future; use a different name as an "
    "identifier\n",
    /*174*/ "symbol '%s' is a type and cannot be used as a value\n",
    /*175*/ "constructors cannot be static\n",
    /*176*/ "non-static method or property '%s' must be called with a value of type '%s'\n",
    /*177*/ "static method '%s' must be invoked via its type (try '%s.%s')\n",
    /*178*/ "cannot coerce %s[] to %s[]; storage classes differ\n",
    /*179*/ "cannot assign %s[] to %s[], storage classes differ\n",
    /*180*/ "function return type differs from prototype. expected '%s', but got '%s'\n",
    /*181*/ "function argument named '%s' differs from prototype\n",
    /*182*/ "functions that return arrays cannot be used as callbacks\n",
};

static const char* fatalmsg[] = {
    /*183*/ "cannot read from file: \"%s\"\n",
    /*184*/ "cannot write to file: \"%s\"\n",
    /*185*/ "table overflow: \"%s\"\n",
    /* table can be: loop table
           *               literal table
           *               staging buffer
           *               option table (response file)
           *               peephole optimizer table
           */
    /*186*/ "insufficient memory\n",
    /*187*/ "invalid assembler instruction \"%s\"\n",
    /*188*/ "numeric overflow, exceeding capacity\n",
    /*189*/ "compiled script exceeds the maximum memory size (%ld bytes)\n",
    /*190*/ "too many error messages on one line\n",
    /*191*/ "codepage mapping file not found\n",
    /*192*/ "invalid path: \"%s\"\n",
    /*193*/ "assertion failed: %s\n",
    /*194*/ "user error: %s\n",
    /*195*/ "compiler bug: calling stock \"%s\" that has no generated code\n",
    /*196*/
    "deprecated syntax; see https://wiki.alliedmods.net/SourcePawn_Transitional_Syntax#Typedefs\n",
};

static const char* warnmsg[] = {
    /*200*/ "symbol \"%s\" is truncated to %d characters\n",
    /*201*/ "redefinition of constant/macro (symbol \"%s\")\n",
    /*202*/ "number of arguments does not match definition\n",
    /*203*/ "symbol is never used: \"%s\"\n",
    /*204*/ "symbol is assigned a value that is never used: \"%s\"\n",
    /*205*/ "redundant code: constant expression is zero\n",
    /*206*/ "redundant test: constant expression is non-zero\n",
    /*207*/ "unknown #pragma\n",
    /*208*/ "function with tag result used before definition, forcing reparse\n",
    /*209*/ "function \"%s\" should return a value\n",
    /*210*/ "possible use of symbol before initialization: \"%s\"\n",
    /*211*/ "possibly unintended assignment\n",
    /*212*/ "possibly unintended bitwise operation\n",
    /*213*/ "tag mismatch\n",
    /*214*/ "possibly a \"const\" array argument was intended: \"%s\"\n",
    /*215*/ "expression has no effect\n",
    /*216*/ "nested comment\n",
    /*217*/ "loose indentation\n",
    /*218*/ "old style prototypes used with optional semicolumns\n",
    /*219*/ "local variable \"%s\" shadows a variable at a preceding level\n",
    /*220*/ "expression with tag override must appear between parentheses\n",
    /*221*/ "label name \"%s\" shadows tag name\n",
    /*222*/ "number of digits exceeds rational number precision\n",
    /*223*/ "redundant \"sizeof\": argument size is always 1 (symbol \"%s\")\n",
    /*224*/ "user warning: %s\n",
    /*225*/ "unreachable code\n",
    /*226*/ "a variable is assigned to itself (symbol \"%s\")\n",
    /*227*/ "more initializers than enum fields\n",
    /*228*/ "length of initializer exceeds size of the enum field\n",
    /*229*/ "index tag mismatch (symbol \"%s\")\n",
    /*230*/ "unused230\n",
    /*231*/ "unused231\n",
    /*232*/ "output file is written, but with compact encoding disabled\n",
    /*233*/ "unused233\n",
    /*234*/ "symbol \"%s\" is marked as deprecated: %s\n",
    /*235*/ "public function lacks forward declaration (symbol \"%s\")\n",
    /*236*/ "unknown parameter in substitution (incorrect #define pattern)\n",
    /*237*/
    "coercing functions to and from primitives is unsupported and will be removed in the future\n",
    /*238*/ "'%s:' is an illegal cast; use view_as<%s>(expression)\n",
    /*239*/ "'%s' is an illegal tag; use %s as a type\n",
    /*240*/ "'%s:' is an old-style tag operation; use view_as<%s>(expression) instead\n",
};
