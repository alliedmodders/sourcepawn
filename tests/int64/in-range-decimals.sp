#include <shell>

public void main() {
	int64 a = 2147483648; // 2^31
	int64 b = PowerInt(2, 31);
	printnum64(a);
	printnum64(b);
	
	int64 c = 4294967295; // 2^32-1
	int64 d = PowerInt(2, 32)-1;
	printnum64(c);
	printnum64(d);
	
	int64 e = 1024; // 2^10
	int64 f = 9223372036854775807; // 2^63-1
	printnum64(e);
	printnum64(f);
}

int64 PowerInt(int base, int exponent)
{
	int64 result = 1;
	for (int i = 0; i < exponent; i++)
		result *= base;
	return result;
}
