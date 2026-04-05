#include <shell>

public void main()
{
	float v = 1.0;
	for (int i = 0; i < 30; i++) {
		if (v < -1.0) {
			print("got here:");
			printfloat(v);
		} else {
			print("got there:");
			printfloat(v);
		}
		v -= 0.1;
	}
}
