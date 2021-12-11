#include <shell>

Handle g_handle[1];

public void main()
{
	delete (GetHandle())[0];
}

Handle[] GetHandle()
{
	return g_handle;
}
