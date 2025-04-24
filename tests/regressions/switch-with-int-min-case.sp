#include <shell>

public void main()
{
	func(-1);
	func(0);
	func(1);
	func(2);
	func(3);
	func(4);
	func(-2147483648);
	print("---\n");
	func2(-2147483648);
	func2(-2147483647);
	func2(2147483647);
}

void func2(int a1) {
	switch (a1) {
		case -2147483648:
			print("-2147483648\n");
		case -2147483647:
			print("-2147483647\n");
		default:
			print("default\n");
	}
}

void func(int a1)
{
	switch (a1)
	{
		case -1:
		{
			print("case: -1\n");
		}
		case 0:
		{
			print("case: 0\n");
		}
		case 1:
		{
			print("case: 1\n");
		}
		case 2:
		{
			print("case: 2\n");
		}
		case 3:
		{
			print("case: 3\n");
		}
		case (-2147483648):
		{
			print("case: -2147483648\n");
		}
	

		default:
		{
			print("case: default\n");
		}
	}
}
