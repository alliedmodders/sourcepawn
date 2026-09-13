#include <shell>

char gFill[10] = {'_', ...};
char gPartial[10] = {'a', 'b'};
char gSingle[10] = {'x'};

static void Dump(const char[] title, const char[] buf)
{
  print(title);
  print("\n");
  for (int i = 0; i < 10; i++)
    printnum(buf[i]);
}

static void DirtyStack()
{
  char dirty[128];
  for (int i = 0; i < 128; i++)
    dirty[i] = 'Z';
}

static void CheckLocals()
{
  char fill[10] = {'_', ...};
  Dump("local fill", fill);

  char partial[10] = {'a', 'b'};
  Dump("local partial", partial);

  char single[10] = {'x'};
  Dump("local single", single);

  static char sFill[10] = {'_', ...};
  Dump("static fill", sFill);
}

public main()
{
  Dump("global fill", gFill);
  Dump("global partial", gPartial);
  Dump("global single", gSingle);

  DirtyStack();
  CheckLocals();
}
