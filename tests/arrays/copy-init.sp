#include <shell>

int g_src[3] = { 10, 20, 30 };
int g_dst[3] = g_src;

int g2_src[2][3] = {{1, 2, 3}, {4, 5, 6}};
int g2_dst[2][3] = g2_src;

void TakeConst(const int src[3])
{
  int dst[3] = src;
  printnum(dst[0]);
  printnum(dst[1]);
  printnum(dst[2]);

  dst[0] = 77;
  printnum(dst[0]);
  printnum(src[0]);
}

public main()
{
  int src[3] = { 1, 2, 3 };
  int dst[3] = src;
  printnum(dst[0]);
  printnum(dst[1]);
  printnum(dst[2]);

  // The copy must be independent of the source.
  src[0] = 99;
  printnum(dst[0]);

  // Globals.
  printnum(g_dst[0]);
  printnum(g_dst[1]);
  printnum(g_dst[2]);

  // Fixed-size heap array (too large to be a flat array).
  int big_src[2000];
  big_src[0] = 11;
  big_src[1999] = 22;
  int big_dst[2000] = big_src;
  printnum(big_dst[0]);
  printnum(big_dst[1999]);
  big_src[0] = 33;
  printnum(big_dst[0]);

  printnum(g2_dst[0][0]);
  printnum(g2_dst[1][2]);

  TakeConst(src);

  // A smaller char array copies the remaining elements as zero.
  char small[4] = "hi";
  char big[8] = small;
  for (int i = 0; i < 8; i++)
    printnum(big[i]);
}
