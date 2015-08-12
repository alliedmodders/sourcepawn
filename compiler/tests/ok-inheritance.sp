native CloseHandle(Handle:handle);

methodmap Handle {
	public native void Close();
};

methodmap Crab < Handle {
};

public main()
{
	new Crab:x;
	x.Close();
}
