// Typesets create a legacy funcenum_t with multiple entries
typeset MyCallback {
    function void (int param);
    function void (int param, int param2);
};

void MyFunc(int param) {}

public void OnPluginStart() {
    // This should correctly cast the function to a legacy funcid (OP_GETFUNCID)
    // and pass VM validation.
    MyCallback cb = MyFunc;
    #pragma unused cb
}
