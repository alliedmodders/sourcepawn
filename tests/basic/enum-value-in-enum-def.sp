#include <shell>

enum eTest
{
  eTest1 = 1 << 0,
  eTest2 = 1 << 1,
  eTest3 = 1 << 2,
  eTest4 = 1 << 3,
  
  eTest5 = eTest1,
  eTest6 = eTest2,

  eTest7 = eTest5 | eTest6,
};

public void main() {
  printnum(eTest1);
  printnum(eTest2);
  printnum(eTest3);
  printnum(eTest4);
  printnum(eTest5);
  printnum(eTest6);
  printnum(eTest7);
}
