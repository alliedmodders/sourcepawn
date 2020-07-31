#include <shell>

char str[64] = "Hello" ... ' ' ... "World" ... '\x21' ... '\n';

public main() {
	print(str);
}
