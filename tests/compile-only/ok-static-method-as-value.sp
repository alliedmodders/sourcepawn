// warnings_are_errors: true
#pragma newdecls required
#pragma semicolon 1

native void PrintToServer(const char[] fmt, any ...);

// Legacy typedef, as used by SM-style native APIs.
typedef LegacyCallback = function void (int n);
native void CallWithNative(LegacyCallback fn, int n);
native void CallWithFunction(Function fn);

// Modern typedef, callable directly.
typedef Callback = (int n) -> void;

methodmap MyActions {
    public static void MyCallback(int n) {
        PrintToServer("%d", n);
    }
}

void Relay(Callback fn, int n) {
    fn(n);
}

public void main()
{
    // Pass a static method as a callback argument (issue #1119).
    CallWithNative(MyActions.MyCallback, 1);

    // Store it in a typed funcref variable, then call through it.
    Callback cb = MyActions.MyCallback;
    cb(2);

    // Forward it through another function.
    Relay(MyActions.MyCallback, 3);

    // Legacy Function-tag coercion, like Call_StartFunction uses.
    Function f = MyActions.MyCallback;
    CallWithFunction(f);
    CallWithFunction(MyActions.MyCallback);
}
