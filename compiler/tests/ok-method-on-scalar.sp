native CloneHandle(Handle:handle);
native CloseHandle(Handle:handle);

methodmap Handle {
	public native void Clone();
	public native void Close();
};

public main()
{
	new Handle:x;
	x.Close();
}
