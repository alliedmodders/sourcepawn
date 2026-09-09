native intptr GetIntPtr();

public void main()
{
	int x[sizeof(GetIntPtr())];
}
