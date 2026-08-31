typedef Callback = () -> void;
native void NativeVarargs(any ...);

public void main() {
    Callback arr[2];
    NativeVarargs(arr);
}
