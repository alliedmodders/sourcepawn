native void printnums(...);

int Array1[][] = { {1}, {2, 3}, {4, 5, 6} };

public main()
{
  printnums(sizeof(Array1), sizeof(Array1[]));
}
