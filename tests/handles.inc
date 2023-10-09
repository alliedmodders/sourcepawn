// vim: set ts=2 sw=2 tw=99 et:

enum Handle { INVALID_HANDLE = 0 }

methodmap Handle __nullable__ {
    public native ~Handle();
    public native void Close();
};
