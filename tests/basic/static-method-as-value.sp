#include <shell>

methodmap MyActions {
    public static void Callback() {
        print("callback\n");
    }
}

public void main() {
    // Pass a static method straight into a callback parameter.
    printnum(execute(MyActions.Callback, 1));

    // Store it in a typed funcref variable first.
    InvokeCallback fn = MyActions.Callback;
    printnum(execute(fn, 2));

    // Two references to the same static method must be equal.
    printnum(fn == MyActions.Callback);
}
