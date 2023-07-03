native int Foo(int a=1);
char g_sStr[256];
public void ArrayCondTest(const char[] arg1)
{
	if (g_sStr[0])  // I want to check if string not empty
		Foo();
	//  If i forgot to index first char, I will get error
	//if (g_sStr)  //  error 033: array must be indexed (variable "g_sStr")
	//	Foo();
	if (g_sStr || Foo()) // Compiled without errors; Actually check if g_sStr[0] **address** not 0 - always true
		Foo();
	// "if (g_sStr || Foo())" bytecode
	//3548: break // 3512
	//3552: const.pri 2280
	//3560: jnz 3600
	//3568: const.pri 1
	//3576: push.pri
	//3580: sysreq.n 2 1
	//3592: jzer 3628
	//3600: break // 3560
	//3604: const.pri 1
	//3612: push.pri
	//3616: sysreq.n 2 1
	//3628: break // 3592
	char localStr[16];
	if (Foo() && (localStr) && Foo())  // Compiled without errors
		Foo();
	bool cond = Foo() && arg1;  // Compiled without errors
	if (cond)
		Foo();
}
