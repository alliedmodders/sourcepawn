#include <shell>

// The JIT's LL_TEST_F32/LL_TEST_F64 (used for ! and !! on floats/doubles in
// value context) emitted "set(not_zero, al)" followed by a full-register
// store, leaving stale garbage in bits 8-31 of the result. The fix is a
// movzxb before the store. See jit_x64.cpp EmitUnaryFloatOp and
// jit_x86.cpp EmitUnaryDoubleOp.
public void main()
{
    float f0 = 0.0;
    float f1 = 1.0;
    double d0 = 0.0d;
    double d1 = 1.0d;
    printnums(8, !f0, !f1, !!f0, !!f1, !d0, !d1, !!d0, !!d1);

    bool b0 = !!f0;
    bool b1 = !f1;
    bool b2 = !d0;
    bool b3 = !!d1;
    printnums(4, b0, b1, b2, b3);

    if (f0) {
        print("bad\n");
    }
    if (!d0) {
        print("ok\n");
    }
}
