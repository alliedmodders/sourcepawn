native void printnum(int num);

using __intrinsics__.Handle;

methodmap Crab < Handle
{
	public Crab() {
		return Crab:2;
	}
};

public main()
{
	Crab egg = new Crab();
	printnum(_:egg);
}
