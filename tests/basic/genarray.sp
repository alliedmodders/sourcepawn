#include <shell>

public main()
{
  int l1 = 10;
  int l2 = 20;

  print("simple genarray\n");
  int[] simple = new int[l1];
  for (int i = 0; i < 10; i++) {
    simple[i] = i * 10;
    printnum(simple[i]);
  }

  print("decl complex genarray\n");
  decl garbage[l1][l2];
  garbage[4][5] = 6;
  printnum(garbage[4][5]);

  print("new complex genarray\n");
  int[][] notgarbage = new int[l1][l2];
  notgarbage[3][5] = garbage[4][5];
  printnums(notgarbage[0][0], notgarbage[3][5]);

  char[] str = new char[l1];
  str[0] = 'h';
  str[1] = 'e';
  str[2] = 'l';
  str[3] = 'l';
  str[4] = 'o';
  str[5] = '\n';
  print(str);
  
  char[] strinit = "this is initialized\n";
  print(strinit);
}
