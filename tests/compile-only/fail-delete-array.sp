#include <shell>

Handle timers[200];
Handle arr[5][5][5][5][5];
char s[2][4] = { "foo", "bar" };

public void main()
{
	char s2[][] = { "baz", "quux" };
	char[][] s3 = new char[4][4];
	Handle[][][][][] arr2 = new Handle[5][5][5][5][5];

	delete arr[0][0][0][0][0];
	delete arr[0][0][0][0];
	delete arr[0][0][0];
	delete arr[0][0];
	delete arr[0];
	delete arr;
	delete s;
	delete s[1];
	delete s[1][1];
	delete arr2[0][0][0][0][0];
	delete arr2[0][0][0][0];
	delete arr2[0][0][0];
	delete arr2[0][0];
	delete arr2[0];
	delete arr2;
	delete s2;
	delete s2[1];
	delete s2[1][1];
	delete s3;
	delete s3[1];
	delete s3[1][1];
	delete timers;
}
