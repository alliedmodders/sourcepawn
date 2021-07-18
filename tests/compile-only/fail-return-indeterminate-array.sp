native void PrintToServer(const char[] fmt, any:...);

stock char[] Workaround(val)
{
  char sNames[][] = {"Humans", "Zombies", "Egg"};
  return sNames[val];
}

public OnPluginStart()
{
  PrintToServer("Good: %s", Workaround(1));
}
