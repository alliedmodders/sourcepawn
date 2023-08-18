#include <shell>

public void main()
{
  if (1)
  {
    if (0)
    {
      return;
    }

    print("hi\n");
  }
  else
  {
    print("this should never happen\n");
  }
}
