// warnings_are_errors: true
#include <handles>

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
