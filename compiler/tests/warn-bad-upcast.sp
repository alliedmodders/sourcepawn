native CloseHandle(Handle:handle);

methodmap Handle {
	public native void Close();
};

methodmap Crab {
};

public main()
{
	new Crab:x;
	new Handle:y;
	x = y;
}
