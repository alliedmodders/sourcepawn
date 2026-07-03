#include <shell>

// Contiguous case values compile to a dense jump table in the JIT.
// Cases sharing one body make multiple table entries reference the same label.
public void main()
{
  testContiguousShared(6);
  testContiguousShared(5);
  testContiguousShared(4);
  testContiguousShared(3);
  testContiguousShared(2);
  testContiguousShared(1);
  testContiguousShared(0);
  testContiguousShared(7);
}

void testContiguousShared(int n)
{
  switch (n) {
  case 0:
    print("first\n");
  case 1,2,3:
    print("second\n");
  case 4,5:
    print("third\n");
  case 6:
    print("fourth\n");
  default:
    print("default\n");
  }
  print("fallthrough\n");
}
