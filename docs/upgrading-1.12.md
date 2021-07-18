SourcePawn 1.12
---------------

## Assignment Changes

The following code is no longer valid:

    // error 017: undefined symbol "i"
    int i = i;

In previous versions of SourcePawn, the right-hand side of the expression was
evaluated after creating the left-hand name. We have changed the ordering to
eliminate a source of confusion and possible bugs. Because the right-hand side
now evaluates first, the symbol `i` does not exist.

The fix is to explicitly initialize to zero:

    int i = 0;

## Return Value Changes

It is now a warning to omit a return statement. That means the following
examples are all problematic:

    // Should return an explicit value
    public Action OnTimer() {
    }

    // Should return an explicit value
    public int GetNumber() {
        if (!DoStuff()) return;
    }

To ease the transition to the new semantic checker, spcomp will emit a warning
when the return value is an enum, boolean, or integer. Any other type is an
error and compilation will fail.

## Syntax Changes

Two obscure syntax options have been deprecated, and will emit a warning. Using
the keyword `do` before a statement, and omitting parenthesis on control
conditionals.

    for int i = 0; i < 10; i++ do {
    }

    switch i do {
    }

    do {
    } while i;

## Array Changes

Type checking of arrays has been greatly improved in SourcePawn 1.12. More
errors will be caught and more initialization patterns are supported. Below are
some common errors that the new compiler finds and some suggestions as to how
to fix them.

### error 183: brackets after variable name indicates a fixed-size array, but size is missing or not constant

The following code will trigger this error:

    stock void DoCommand(int client, char argument[] = "")

When brackets `[]` follow the variable name, they indicate a fixed size. When
they follow the type, they indicate a dynamic or unknown size. The error in
this example is that the position indicates a fixed size, but no size was
given. There is no way to deduce the size automatically because it's an
argument, and the initializer only applies to default values.

The fix is to use the correct bracket positioning:

    stock void DoCommand(int client, char[] argument= "")

### error 047: array sizes do not match, or destination array is too small

There are a number of reasons why this error might appear in 1.12 in a script
that compiled fine in earlier versions.

#### String is too big

Earlier versions of SourcePawn incorrectly computed the size of string arrays.
For example, `char x[3]` was internally calculated as four bytes. This has been
fixed in 1.12. Plugins relying on this incorrect calculation will no longer
compile.

Here are some examples of code that used to work, but don't anymore:

    char a[1] = "0"; // Error: "0" is two bytes

    char b[5];
    b = IsClientInGame(client) ? "true" : "false"; // Error: "false" is 6 bytes

    char buffer[4];
    GeocipCode2(ip, buffer); // Error: char[3] is needed, but user gave char[4]

For international plugins, remember that utf8 strings can occupy more bytes
than characters. You can use sizeof() to check the true size of a string.

#### Multi-dimensional with initializers

The following code will no longer work and will report error 47:

    char WeaponNames[][] = {"awp", "rocket_launcher"};
    char weapon[32] = WeaponNames[0]

The reason is that the final rank of the array has two different sizes, 4 bytes
(for `"awp"`) and 16 bytes (for `"rocket_launcher"`). Therefore the size of the
last dimension is not known at compile-time. There are a few ways to work
around this.

First, you can use a hardcoded size:

    char WeaponNames[][32] = {"awp", "rocket_launcher"};
    char weapon[32] = WeaponNames[0]

Second, you can use `strcopy`:

    char WeaponNames[][] = {"awp", "rocket_launcher"};
    char weapon[32]
    strcopy(weapon, sizeof(weapon), WeaponNames[0])

### warning 241: scalar assignment to array is deprecated

This warning will happen when the following pattern is detected:

    int gNumKills[MAXPLAYERS + 1] = 0;

The error is that `0` is a scalar (a single value) being assigned to an array,
which is illegal. The correct syntax is:

    int gNumKills[MAXPLAYERS + 1] = {0, ...};

For `0` values (or `false`, or `0.0`, or `""`), an initializer is not needed at all:

    int gNumKills[MAXPLAYERS + 1];

Since this pattern is quite common, we special-cased it so that it still works
in 1.12 with a warning. In future versions it may become an error.

### error 101: fixed dimensions must be after the array name, not on the type

The following code will trigger this error:

    void GetVector(float[3] pos) {}

The brackets (`[]`) are in the wrong position. Next to the type, they indicate
an array of unknown or any size. Next to the name, they indicate a fixed size.
The correct syntax is:

    void GetVector(float pos[3]) {}

### error 165: cannot create dynamic arrays in static scope - did you mean to create a fixed-length array with brackets after the variable name?

The following code will trigger this error:

    static float[3] sVector

This is similar to error 101 above. The brackets next to the type indicate an
array of unknown size. When declaring a local variable in legacy syntax, the
`3` indicates a dynamic array. Dynamic arrays aren't allowed in global or
static scope. The correct syntax is:

    static float sVector[3]

## C++ Changes

Plugins compiled with SourcePawn 1.12 use a new internal representation for
multi-dimensional arrays called "direct arrays". A new method,
`IPluginRuntime::UsesDirectArrays`, has been added to detect this. For
extensions that provide natives taking multi-dimensional arrays or invoke
public functions with multi-dimensional arrays, it is necessary to check for
direct arrays and just memory access appropriately.
