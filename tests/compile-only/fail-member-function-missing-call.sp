#pragma semicolon 1
#pragma newdecls required

methodmap Test 
{
	public int GetNum()
	{
		return 1;
	}
}

public int main()
{
	Test test;
	int num = test.GetNum;  // problematic code. should be: test.GetNum();
	return num;
}
