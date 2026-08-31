native void printdouble(double n);

double sGlobal = 2.5d;

public main()
{
  double local = 5.5d;
  double array[5] = {3.0d, 7.0d, 5.0d, 9.0d, 11.0d};
  int index = 2;

  printdouble(sGlobal++);
  printdouble(++sGlobal);
  printdouble(sGlobal--);
  printdouble(--sGlobal);

  printdouble(local++);
  printdouble(++local);
  printdouble(local--);
  printdouble(--local);

  printdouble(array[index]++);
  printdouble(++array[index]);
  printdouble(array[index]--);
  printdouble(--array[index]);

  testArgs(array, local);
  printdouble(local);
  printdouble(array[3]);
  printdouble(array[4]);
}

void testArgs(double[] array, double& addr)
{
  printdouble(array[4]++);
  printdouble(++array[4]);
  printdouble(array[4]--);
  printdouble(--array[4]);

  int index = 3;
  printdouble(array[index]++);
  printdouble(++array[index]);
  printdouble(array[index]--);
  printdouble(--array[index]);

  index = 1;

  printdouble(addr++);
  printdouble(++addr);
  printdouble(addr--);
  printdouble(--addr);
}
