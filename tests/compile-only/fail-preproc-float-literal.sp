// type: compiler-output
#define INTERVAL 0.3

public main()
{
#if INTERVAL
	PrintToServer("hi");
#endif
}

public OnPluginStart() { main(); }
