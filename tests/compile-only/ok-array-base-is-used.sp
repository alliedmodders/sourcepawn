// warnings_are_errors: true
native void PrintToServer(const char[] fmt, any:...);

public void OnPluginStart()
{
	int stuff[4];
	FillArray(stuff);
	PrintToServer("{%d, %d, %d, %d}", stuff[0], stuff[1], stuff[2], stuff[3]);
}

void FillArray(int stuff[4])
{
	stuff = { 5, 2, 8, 1 };	// warning 204: symbol is assigned a value that is never used: "stuff"
}
