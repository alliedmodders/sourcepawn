typedef Callback = () -> void;

enum struct EStruct {
    Callback cb;
}

native void NativeEnumStruct(EStruct s);

public void main() {}
