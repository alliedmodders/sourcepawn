native void PrintToServer(const char[] egg);

methodmap B {
  public static void TestB() {
  	A.TestA();
  	PrintToServer("TestB");
  }
}

methodmap A {
  public static void TestA() {
  	PrintToServer("TestA");
  }
}

public main()
{
}
