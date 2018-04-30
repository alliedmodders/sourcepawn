native CloseHandle(Handle:handle);

enum Handle {
	INVALID_HANDLE = 0,
};

methodmap Handle {
	public native void Close();
};

public main()
{
	INVALID_HANDLE.Close();
}
