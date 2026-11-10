// warnings_are_errors: true
#pragma newdecls required
#pragma semicolon 1

typedef VoidCallback = function void ();
native void CallWith(VoidCallback fn);

methodmap MyActions {
    public static void MyCallback() {
    }
}

public void main()
{
    CallWith(MyActions::MyCallback);
}
