methodmap M
{
	public static native void Ping();
}

native void CallVariadic(any...);

public void main()
{
	M.Ping();
	CallVariadic(0, M.Ping, M.Ping);
}
