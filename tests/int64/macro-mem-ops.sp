#include <shell>

int64 sGlobal = 22;

public main()
{
  int64 local = 55;
  int64 array[5] = {3, 7, 5, 9, 11};
  int index = 2;

  print("globals\n");
  printnum64(sGlobal++);
  printnum64(++sGlobal);
  printnum64(sGlobal--);
  printnum64(--sGlobal);

  print("locals\n");
  printnum64(local++);
  printnum64(++local);
  printnum64(local--);
  printnum64(--local);

  print("arrays\n");
  printnum64(array[index]++);
  printnum64(++array[index]);
  printnum64(array[index]--);
  printnum64(--array[index]);

  testArgs(array, local);
  printnum64(local);
  printnum64(array[3]);
  printnum64(array[4]);
}

void testArgs(int64[] array, int64& addr)
{
  print("refarrays, const index\n");
  printnum64(array[4]++);
  printnum64(++array[4]);
  printnum64(array[4]--);
  printnum64(--array[4]);

  int index = 3;
  print("refarrays, var index\n");
  printnum64(array[index]++);
  printnum64(++array[index]);
  printnum64(array[index]--);
  printnum64(--array[index]);

  index = 1;

  print("ref\n");
  printnum64(addr++);
  printnum64(++addr);
  printnum64(addr--);
  printnum64(--addr);
}
