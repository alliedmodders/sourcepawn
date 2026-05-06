public void OnPluginStart()
{
    char buff[] = "\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test\
        test";
    #pragma unused buff

    PrintToServer(1); // the actual location of the error - 25
}
