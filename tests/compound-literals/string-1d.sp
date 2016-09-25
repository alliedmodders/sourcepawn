#include <shell>

char EmptyString[] = "";
char BigString[] = "The quick brown dog jumped over the lazy frog. X.\n";
char FixedString[10] = "Hi!\n";

public main()
{
  print(EmptyString);
  print("\n");
  print(BigString);
  print(FixedString);
  printnums(sizeof(EmptyString), sizeof(BigString), sizeof(FixedString));
}
