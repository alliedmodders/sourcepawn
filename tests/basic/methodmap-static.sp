#include <shell>

methodmap X
{
	public static int GetThing() {
		return 10;
	}
}

public main() {
	printnum(X.GetThing());
}
