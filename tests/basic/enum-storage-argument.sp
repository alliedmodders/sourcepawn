#include <shell>

enum Test {
	Float:vec3[3],
	arr4[4]
}

public main()
{
  int test[Test];
  float v[3];
  
  printfloat(func1(test[vec3]));
  printfloat(func1(v));
  printnum(func2(test[arr4]));
}

float func1(float a[3])
{
	return a[0];
}

int func2(int a[4])
{
	return a[1];
}
