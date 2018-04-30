using __intrinsics__.Handle;

methodmap StringMap < Handle
{
}

native Log(const char[] fmt, any:...)

public main()
{
	StringMap f
	Log("hello %d", f != null)
}
