#include <shell>

public main()
{
  frame1();
}

void frame1()
{
  frame2();
}

void frame2()
{
  dump_stack_trace();
}
