// Regression test for alliedmodders/sourcepawn#1197 and #1194. A view_as<>
// (or legacy tag cast) between int and float over an rvalue, such as a call
// result, emitted no instruction, so the value kept its old type on the
// stack and the surrounding operator lowered to the wrong kind of
// arithmetic: view_as<float>(call()) * 2.0 was an integer multiply.

native void printnum(int x);
native void printfloat(float x);
native int dynamic_native(int arg);

stock int IntFn(int x) {
    return x;
}

stock any AnyFn(any x) {
    return x;
}

stock float FloatFn(float x) {
    return x;
}

public void main() {
    // #1197: native and script call results reinterpreted as float.
    printfloat(view_as<float>(dynamic_native(view_as<int>(2.5))) * 2.0);
    printfloat(view_as<float>(dynamic_native(view_as<int>(2.5))) + 1.0);
    printfloat(view_as<float>(IntFn(view_as<int>(2.5))) * 2.0);
    printfloat(view_as<float>(AnyFn(2.5)) * view_as<float>(AnyFn(4.0)));
    printnum(view_as<float>(IntFn(view_as<int>(-2.5))) < -3.0);

    // #1194: legacy tag cast on the right of a compound assignment.
    float w = 2.0;
    w *= Float:AnyFn(1.5);
    printfloat(w);
    w = Float:AnyFn(1.0) + Float:AnyFn(0.25);
    printfloat(w);

    // The other direction: float call results reinterpreted as int.
    printnum(view_as<int>(FloatFn(2.5)) - view_as<int>(FloatFn(2.0)));
    printnum(view_as<int>(FloatFn(2.5)) + 1);
}
