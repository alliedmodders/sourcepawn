//#include <shell>
native void printnum(int x);

public main()
{
  static String:triggers[][] = { "gun", "!gun", "/gun", "guns", "!guns", "/guns", "menu", "!menu", "/menu", "weapon", "!weapon", "/weapon", "weapons", "!weapons", "/weapons" };
  static triggerCount = sizeof(triggers);
  printnum(triggerCount);
}
