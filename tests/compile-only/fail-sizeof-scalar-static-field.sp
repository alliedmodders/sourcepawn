enum struct ES
{
	float f;
	intptr p;
}

public void main()
{
	int a[sizeof(ES::f)];
	int b[sizeof(ES::p)];
}
