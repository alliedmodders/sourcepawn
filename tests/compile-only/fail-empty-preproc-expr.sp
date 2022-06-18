#define FOO

public void OnPluginStart()
{
	#if FOO
	PrintToServer("Foo");
	#endif
}
