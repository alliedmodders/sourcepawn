// This creates a funcenum_t under the hood with exactly 1 entry.
typedef MyLegacyCallback = function void (int param);

// Native function that takes the legacy function type with INVALID_FUNCTION as default.
native void MyLegacyNative(MyLegacyCallback cb = INVALID_FUNCTION);

// Dummy callback function
void MyCallback(int param) {}

public void OnPluginStart() {
    // Calling with default argument tests that INVALID_FUNCTION (0) passes verification.
    MyLegacyNative();

    // Calling with actual callback tests that OP_GETFUNCID passes verification.
    MyLegacyNative(MyCallback);
}
