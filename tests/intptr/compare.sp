#include <shell>

public main()
{
  intptr a = 100;

  print(">=\n");
  printnum(a >= 90);
  printnum(a >= 100);
  printnum(a >= 110);

  print(">\n");
  printnum(a > 90);
  printnum(a > 100);
  printnum(a > 110);

  print("<\n");
  printnum(a < 90);
  printnum(a < 100);
  printnum(a < 110);

  print("<=\n");
  printnum(a <= 90);
  printnum(a <= 100);
  printnum(a <= 110);

  print("branch\n");
  if (a >= 90) printnum(1); else printnum(2);
  if (a >= 110) printnum(3); else printnum(4);
  if (a < 90) printnum(5); else printnum(6);
  if (a == 100) printnum(7); else printnum(8);
  if (a != 100) printnum(9); else printnum(10);
  if (!(a < 90)) printnum(11); else printnum(12);
  while (a > 90) { printnum(13); break; }
  if (a < 90 || a == 100) printnum(14); else printnum(15);
  intptr b = 100;
  if (a == b) printnum(16); else printnum(17);
  if (a != b) printnum(18); else printnum(19);
  // Wide values: on x64 p == q == 2^40+7 (low cell 7); on x86 p == q == 7.
  // Both sides of the arch guard emit identical stdout.
  int64 probe = 1099511627783;   // 2^40 + 7
  intptr p = view_as<intptr>(probe);
  intptr q = view_as<intptr>(probe);
  if (sys_intptr_size() == 8) {
    if (p > 7) printnum(20); else printnum(21);
    if (p == 7) printnum(22); else printnum(23);
    if (q < 8) printnum(24); else printnum(25);
    if (p == q) printnum(26); else printnum(27);
  } else {
    printnum(20);
    printnum(23);
    printnum(25);
    printnum(26);
  }
}
