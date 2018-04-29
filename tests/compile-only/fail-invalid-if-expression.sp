native void PrintToChatAll(const char[] str);

public OnPluginStart()
{
	if(true, "must_be_a_string")
	{
		PrintToChatAll("Internal error :(");
	}
}
