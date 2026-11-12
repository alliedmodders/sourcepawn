// warnings_are_errors: true
//
// Regression test for issue #865: an unused, deprecated stock function that
// calls other deprecated symbols must not produce warnings, even though its
// body is still fully analyzed.

#pragma semicolon 1
#pragma newdecls required

#pragma deprecated Do not use this.
native void DeprecatedNative();

#pragma deprecated This function is deprecated.
stock void UnusedFunction()
{
    DeprecatedNative();
    NestedUnusedFunction();
}

#pragma deprecated This nested function is also deprecated.
stock void NestedUnusedFunction()
{
    DeprecatedNative();
}

public void main()
{
}
