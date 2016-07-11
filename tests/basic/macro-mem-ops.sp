#include <shell>

int sGlobal = 22;

public main()
{
  int local = 55;
  int array[5] = {3, 7, 5, 9, 11};
  int index = 2;
  char str[] = "abc";

  print("globals\n");
  printnum(sGlobal++);
  printnum(++sGlobal);
  printnum(sGlobal--);
  printnum(--sGlobal);

  print("locals\n");
  printnum(local++);
  printnum(++local);
  printnum(local--);
  printnum(--local);

  print("arrays\n");
  printnum(array[index]++);
  printnum(++array[index]);
  printnum(array[index]--);
  printnum(--array[index]);

  index = 1;
  print("char arrays\n");
  printnum(str[index]++);
  printnum(++str[index]);
  printnum(str[index]--);
  printnum(--str[index]);

  testArgs(array, str, local);
  printnums(local, array[3], array[4]);

  index = 0;
  sGlobal = 0;
  printnums(index, sGlobal);
  print(str);
  print("\n");
}

void testArgs(int[] array, char[] str, int& addr)
{
  print("refarrays, const index\n");
  printnum(array[4]++);
  printnum(++array[4]);
  printnum(array[4]--);
  printnum(--array[4]);

  print("ref chararrays, const index\n");
  printnum(str[1]++);
  printnum(++str[1]);
  printnum(str[1]--);
  printnum(--str[1]);

  int index = 3;
  print("refarrays, var index\n");
  printnum(array[index]++);
  printnum(++array[index]);
  printnum(array[index]--);
  printnum(--array[index]);

  index = 1;
  print("ref chararrays, var index\n");
  printnum(str[index]++);
  printnum(++str[index]);
  printnum(str[index]--);
  printnum(--str[index]);

  print("ref\n");
  printnum(addr++);
  printnum(++addr);
  printnum(addr--);
  printnum(--addr);

  addr = 0;
  array[index] = 0;
  array[4] = 0;
  str[2] = 0;
}
