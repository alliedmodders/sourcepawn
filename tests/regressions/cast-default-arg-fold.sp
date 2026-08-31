#include <shell>

// Verify that cast expressions fold into constants when used as default
// arguments. Before the fix, any cast whose target type differed from the
// inner in podLoadSize (or was a function-like / typeset) was downgraded
// from iCONSTEXPR to iEXPRESSION by CastNeedsRvalue, so default-argument
// analysis rejected it with "must be a constant expression" (error 008).

typeset Callback {
    function void ();
};

stock void CallIt(Callback cb = Callback:0) {
    // The default-argument value should be a constant 0 (null function
    // pointer). Reaching this body with the default proves the compiler
    // accepted it.
    printnum(cb ? 1 : 0);
}

stock int ReadInt8(int8 v = int8:127) {
    return v;
}

stock int64 ReadInt64(int64 v = int64:5) {
    return v;
}

stock int ReadInt8OutOfRange(int8 v = int8:300) {
    return v;
}

public main() {
    // Tag-prefixed literal in a default argument — the original bug.
    // Previously rejected with error 008.
    CallIt();

    // Narrowing cast (int8 within range) — should fold to 127.
    printnum(ReadInt8());

    // Widening cast (int -> int64) — should fold to 5 with const_int64_
    // properly populated (no UB from reading an uninitialized union member).
    printnum64(ReadInt64());

    // Narrowing cast (int8 out of range) — must truncate at compile time
    // so the printed value is 44, not 300.
    printnum(ReadInt8OutOfRange());

    // Same casts but with explicit arguments — should still produce the
    // same runtime results, ensuring the cast path itself is unchanged.
    printnum(ReadInt8(int8:127));
    printnum64(ReadInt64(int64:5));
    printnum(ReadInt8OutOfRange(int8:300));
}
