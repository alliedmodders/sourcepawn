// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// Regression test for issue #865: calls to deprecated symbols from inside
// deprecated functions should not warn. Calls from live code must still warn.

#pragma semicolon 1
#pragma newdecls required

#pragma deprecated Do not use this.
native void DeprecatedNative();

#pragma deprecated This function is deprecated.
stock void DeprecatedStock()
{
    // No warning: the enclosing function is itself deprecated.
    DeprecatedNative();
}

#pragma deprecated This function is deprecated.
// Note: defining a deprecated, non-stock function warns at the definition
// site (pre-existing behavior); only the calls inside the body are silent.
void DeprecatedNonStock()
{
    // No warning: the enclosing function is itself deprecated.
    DeprecatedNative();
    DeprecatedStock();
}

void LiveFunction()
{
    // Both of these must still warn.
    DeprecatedNative();
    DeprecatedStock();
}

public void main()
{
    LiveFunction();
    DeprecatedNonStock();
}
