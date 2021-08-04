// warnings_are_errors: true

#pragma deprecated Do not use this
native void DontUse();

#pragma deprecated Do not use this either
forward void Blah();

public main()
{
    DontUse();
}

public void Blah()
{
}
