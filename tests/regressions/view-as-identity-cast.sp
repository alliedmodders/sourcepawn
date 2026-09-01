// Regression test for identity view_as<> casts. view_as<T> over an
// expression that is already T must not emit a conversion instruction.
// The verifier only accepts narrow inputs for cvt.i64, so view_as<int64>
// over an int64 expression failed binary validation with "Instruction
// contained invalid parameter". int8 had the same hole (a redundant
// cvt.i8).

native void printnum(int x);
native void printnum64(int64 x);
native void printnumptr(intptr x);

stock int64 Wide(int64 x) {
    return x * 2;
}

stock int8 Narrow8(int8 x) {
    return x;
}

stock int16 Narrow16(int16 x) {
    return x;
}

stock intptr Ptr(intptr x) {
    return x;
}

public void main() {
    int64 a = 5;

    // The original ICE: view_as<int64> over an int64 rvalue expression.
    printnum64(view_as<int64>(a + 1));
    printnum64(view_as<int64>(Wide(21)));
    int64 b = view_as<int64>(a * a);
    printnum64(b + 1);

    // Identity on an lvalue: address path, must behave like the value.
    view_as<int64>(a) = 10;
    printnum64(view_as<int64>(a));

    // int8 identity: previously emitted a redundant cvt.i8. Sign extension
    // must survive without it.
    int8 c = -5;
    int8 n8 = -100;
    printnum(view_as<int8>(Narrow8(n8)));
    printnum(view_as<int8>(c + 1));
    printnum(view_as<int8>(120 + 9));

    // int16 and intptr identity casts already emitted nothing; pin that.
    int16 n16 = 7;
    printnum(view_as<int16>(Narrow16(n16)));
    printnum(view_as<int16>(-3));
    intptr np = 1234;
    printnumptr(view_as<intptr>(Ptr(np)));
}
