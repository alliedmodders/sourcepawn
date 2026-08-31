public const char DEFAULT_VAL[2] = "x";

native void TestDefaultArg(const char[] val = DEFAULT_VAL);

public void OnPluginStart() {
    TestDefaultArg();
}
