#include <shell>

public void main()
{
	{
		int b;
		printnums(0, (b
			      ? (((b > 0)
				  ? 1
				  : GetTime()) + 1)
			      : 0));
	}
}

int GetTime(int x[2] = {}) {
	return 100;
}
