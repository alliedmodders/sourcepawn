// warnings_are_errors: true
enum Handle { INVALID_HANDLE = 0 };

using __intrinsics__.Handle;

methodmap ArrayList < Handle {
  public native ArrayList();
};

public void OnPluginStart()
{
	ArrayList array = new ArrayList();
	
	if (array != INVALID_HANDLE)	// warning 213: tag mismatch
	{
	}
}
