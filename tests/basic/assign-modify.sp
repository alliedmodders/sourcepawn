#include <shell>

float gVal = 100.0;

methodmap Obj {
  public Obj(int v) {
    return view_as<Obj>(v);
  }
  property float val {
    public get() {
      if (view_as<int>(this) == 555)
        return gVal;
      return 0.0;
    }
    public set(float v) {
      if (view_as<int>(this) == 555)
        gVal = v;
    }
  }
}

void TestInt()
{
  int a = 100;
  int b = 1;
  int c = 3;

  a += 1;
  printnum(a);
  a -= b;
  printnum(a);
  a |= b;
  printnum(a);
  a ^= b;
  printnum(a);
  a &= 0x55555555;
  printnum(a);
  a *= c;
  printnum(a);
  a /= c;
  printnum(a);
  a %= c;
  printnum(a);

  a <<= 3;
  printnum(a);

  a = -1;
  a >>= 2;
  printnum(a);
  a >>>= 2;
  printnum(a);
}

void TestFloat()
{
  float a = 100.0;
  float b = 1.0;
  float c = 3.0;

  a += 1.0;
  printfloat(a);
  a -= b;
  printfloat(a);
  a *= c;
  printfloat(a);
  a /= c;
  printfloat(a);
}

void TestAccessor()
{
  Obj o = Obj(555);
  float b = 1.0;
  float c = 3.0;

  o.val += 1.0;
  printfloat(o.val);
  o.val -= b;
  printfloat(o.val);
  o.val *= c;
  printfloat(o.val);
  o.val /= c;
  printfloat(o.val);

  float vec[3] = {3.0, 4.0, 5.0};
  vec[1] += vec[2];
  printfloat(vec[1]);
}

public main()
{
  TestInt();
  TestFloat();
  TestAccessor();
}
