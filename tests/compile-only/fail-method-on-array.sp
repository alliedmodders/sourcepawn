native CloseHandle(Handle:handle);

methodmap Handle {
	public native void Close();
};

public main()
{
	new Handle:x[2];

	x.Close();
}
