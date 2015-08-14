native void CloseHandle(Handle:hndl);
using __intrinsics__.Handle;

methodmap Crab < Handle
{
	public native Crab();
};

public t()
{
	Crab egg = Crab();
	delete egg;
}
