#include <shell>

public main()
{
  char message1[] = "This is a long message.\n";
  char message2[] = "This is short.\n";

  message1 = message2;
  print(message1);

  message1 = "Also short.\n";
  print(message1);
}
