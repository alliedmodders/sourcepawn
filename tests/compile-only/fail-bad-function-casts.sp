native void print(const char[] x);
native void printnum(int n);

typedef TestAny = function void (any[] b);
typedef TestChar = function void (char[] b);

public void main()
{
	char c[] = "hi";
	any a[] = {1,2,3};

	CharArg(c);
	AnyArg(c); // error 178: cannot coerce char[] to any[]; storage classes differ
	CharArg(a); // error 178: cannot coerce any[] to char[]; storage classes differ
	AnyArg(a);

	TestAny anyArgFunc = CharArg; // fine?!
	TestChar charArgFunc = AnyArg; // fine?!

	anyArgFunc = CharArg;
	charArgFunc = AnyArg;

	TestAnyArg(charArgFunc); // error 100: function prototypes do not match
	TestCharArg(anyArgFunc); // error 100: function prototypes do not match
	TestAnyArg(AnyArg);
	TestAnyArg(CharArg); // fine?!
	TestCharArg(CharArg);
	TestCharArg(AnyArg); // fine?!
}

void TestAnyArg(TestAny f)
{
	f = INVALID_FUNCTION;
}
void TestCharArg(TestChar f)
{
	f = INVALID_FUNCTION;
}

void CharArg(char[] b)
{
	print(b);
} // warning 209: function "CharArg" should return a value
void AnyArg(any[] b)
{
	printnum(b[1]);
} // warning 209: function "AnyArg" should return a value
