// warnings_are_errors: true
enum Handle { INVALID_HANDLE = 0 }
using __intrinsics__.Handle;

methodmap ArrayList < Handle { }
methodmap ArrayLister < ArrayList { }

public void main()
{
    ArrayList al;
    ArrayLister al2;

    if (al != INVALID_HANDLE) { } //1.11 mismatch
    if (INVALID_HANDLE != al) { }
    if (al != al2) { }
    if (al2 != al) { } //1.10, 1.11 mismatch
    if (main != INVALID_FUNCTION) { }
    if (INVALID_FUNCTION != main) { } //this PR error 132
}
