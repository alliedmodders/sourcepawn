enum struct InnerStruct
{
	int num;
}

enum struct OuterStruct
{
	InnerStruct inner;
}

public void OnPluginStart()
{
	OuterStruct myStruct;
	Test(myStruct.inner);
}

void Test(any[] arr)
{
	// no viable conversion from InnerStruct[] to any[]
}
