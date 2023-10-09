#include <handles>

methodmap ArrayList < Handle {
	public ArrayList() { return view_as<ArrayList>(1); }
}

public void OnPluginStart()
{
	bool b;

	// ok
	ArrayList x = b ? new ArrayList() : null;

	// error 132: cannot coerce non-object type ArrayList to object type null_t
	ArrayList y = b ? null : new ArrayList();

	if (x || y) {}
}
