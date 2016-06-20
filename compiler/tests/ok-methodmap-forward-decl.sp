#pragma semicolon 1

methodmap MM1;
methodmap MM2;

methodmap MM1
{
	public MM1()
	{
		return view_as<MM2>(1);
	}
};

methodmap MM2
{
	public MM2()
	{
		return view_as<MM1>(2);
	}
};

public void main() {}
