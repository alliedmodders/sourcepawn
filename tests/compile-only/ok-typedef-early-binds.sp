typedef Address = int64;

native void dummyfunc(Address addr);

public void foo() {
    Address patch;
    dummyfunc(patch + 0x0);
}
