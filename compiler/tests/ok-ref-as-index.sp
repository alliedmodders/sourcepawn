
public void OnPluginStart()
{
	int desiredEggs = 3;
	Egg(desiredEggs);
}

void Egg(int &cntEggs)
{
	int[] MYEGGS = new int[cntEggs];

	// Avoid "unused" warning
	MYEGGS[0] = 1;
}
