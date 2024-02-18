#include <shell>

public main() {
    print(test(true))
    print("\n")
    print(test(false))
    print("\n")
}

char[] test(bool ok)
{
	return ok ? "ok" : "not_ok"
}
