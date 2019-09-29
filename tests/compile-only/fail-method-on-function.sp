// force_old_parser: true
// The error message changes based on parser.
native CloseHandle(Handle:handle);

methodmap Handle {
	public native void Close();
};

public main()
{
	main.Close();
}
