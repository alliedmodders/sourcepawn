#pragma newdecls optional

forward void MyVoidForward();

public void OnPluginStart()
{
    // The forward returns void, but the definition hasn't been parsed yet.
    // Ensure the compiler doesn't incorrectly deduce `any` and emit a pop.
    MyVoidForward();
}

public MyVoidForward()
{
}
