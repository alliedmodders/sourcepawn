enum Foo
{
	Foo1,
	Foo2,
	Foo3
}

enum Bar
{
	Bar1,
	Bar2,
	Bar3
}

public void OnPluginStart()
{
	Foo whatever = Foo2;

	switch (whatever)
	{
		case Bar1: whatever = Foo2;
		case 3: whatever = Foo3;
		case 7.4: whatever = Foo1;
		case Foo3: whatever = Foo3;
	}
}
