methodmap ReturnVal
{
    public ReturnVal()
    {
        // error: missing return
    }
    public bool test()
    {
        // error: missing return
    }
    property bool Prop {
        public get() {
            // error: missing return
        }
    }
}

public void OnPluginStart()
{
    ReturnVal v = ReturnVal();
    v.test();
    bool b = v.Prop;
}
