#include <shell>

public main()
{
  testEmpty(5);
  testDefaultOnly(7);
  testSimple(0);
  testSimple(5);
  testContiguous(3);
  testContiguous(2);
  testContiguous(1);
  testContiguous(0);
  testContiguous(9);
  testScattered(30000);
  testScattered(40000);
  testScattered(2000);
  testScattered(100);
  testScattered(0);
  testScattered(9000000);
  testDenseNonZero(5);
  testDenseNonZero(6);
  testDenseNonZero(7);
  testDenseNonZero(4);
  testDenseNonZero(8);
}

void testEmpty(int n)
{
  switch (n) {
  }

  printnum(n);
}

void testDefaultOnly(int n)
{
  switch (n) {
  default:
    printnum(n);
  }
}

void testSimple(int n)
{
  switch (n) {
  case 0:
    print("zero\n");
  default:
    print("not zero\n");
  }
  print("fallthrough\n");
}

void testContiguous(int n)
{
  switch (n) {
  case 0:
    print("first\n");
  case 1:
    print("second\n");
  case 2:
    print("third\n")
  case 3:
    print("fourth\n");
  default:
    print("default\n");
  }
  print("fallthrough\n");
}

void testScattered(int n)
{
  switch (n) {
  case 0:
    print("first\n");
  case 100:
    print("second\n");
  case 2000:
    print("third\n")
  case 30000,40000:
    print("fourth\n");
  default:
    print("default\n");
  }
  print("fallthrough\n");
}

void testDenseNonZero(int n)
{
  switch (n) {
  case 5:
    print("five\n");
  case 6:
    print("six\n");
  case 7:
    print("seven\n");
  default:
    print("default dense nonzero\n");
  }
  print("fallthrough\n");
}
