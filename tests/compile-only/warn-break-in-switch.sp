// vim: set ts=8 sts=4 sw=4 tw=99 et:
int gValue;

void Warns(int mode)
{
	for (int i = 0; i < 10; i++)
	{
		switch (mode)
		{
			case 1:
			{
				gValue++;
				break;
			}
			default:
			{
				break;
			}
		}
	}
}

void DoesNotWarn(int mode)
{
	for (int i = 0; i < 10; i++)
	{
		switch (mode)
		{
			case 1:
			{
				for (int j = 0; j < 10; j++)
					break;
				continue;
			}
		}
	}
}

public main()
{
	Warns(1);
	DoesNotWarn(1);
}
