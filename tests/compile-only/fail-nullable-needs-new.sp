#include <handles>

methodmap Crab < Handle
{
	public native Crab();
};

public t()
{
	Crab egg = Crab();
	delete egg;
}
