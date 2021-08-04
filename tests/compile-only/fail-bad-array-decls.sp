int[] gInvalid = {1};

public OnPluginStart()
{
  int v = 10;
  int invalid1[v];
}

public OnWhatever()
{
  static int[] invalid3 = {1};
}

public OnPluginEnd()
{
  int[] invalid2 = {1};
}

void invalid_arg1(int invalid[])
{
}

void invalid_arg2(int[] invalid = {1, 2, 3})
{
}
