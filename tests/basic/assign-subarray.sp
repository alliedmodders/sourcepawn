#include <shell>

#define MAXPLAYERS 32
char g_sTest[MAXPLAYERS+1][32];

public void main()
{
  g_sTest[1] = "stuff\n";
  print(g_sTest[1]);
}
