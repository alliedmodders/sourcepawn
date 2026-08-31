#include <shell>

// Regression: a local of `object` type (or an array with `object`
// element type) used to crash the compiler during RTTI emission because
// the built-in `object` type shares TypeKind::Object with class types
// but has no ClassDecl. After the fix, `object` is encoded as the new
// kTopObject opcode and the verifier permits coercing any class
// instance or null into it.

int IsNull(object x) {
    if (x == null)
        return 1;
    return 0;
}

public main() {
    // Original crash case: object-typed local array with initializer.
    object arr[1] = {null};
    int result = IsNull(arr[0]);

    // Standalone object local with null initializer.
    object x = null;
    result += IsNull(x);

    // Pass null literal as object.
    result += IsNull(null);

    printnum(result);
}
