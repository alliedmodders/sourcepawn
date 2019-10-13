// force_new_parser: true
#include <shell>

float gVal = 21.5;

methodmap X {
  property float val {
    public get() { return gVal; }
    public set(float v) { gVal = v; }
  }
};

void local(float b)
{
  printfloat(b++);
  printfloat(++b);
  printfloat(--b);
  printfloat(b--);
}

void ref(float& b)
{
  printfloat(b++);
  printfloat(++b);
  printfloat(--b);
  printfloat(b--);
}

void map(X x)
{
  printfloat(x.val++);
  printfloat(++x.val);
  printfloat(--x.val);
  printfloat(x.val--);
}

void array(float vec[3])
{
  printfloat(vec[0]++);
  printfloat(++vec[0]);
  printfloat(--vec[0]);
  printfloat(vec[0]--);
}

public main()
{
  float b = 21.5;
  local(b);

  b = 21.5;
  ref(b);

  float arr[3] = {21.5, 0.0, 0.0};
  array(arr);

  X x;
  map(x);
}
