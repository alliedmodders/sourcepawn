// warnings_are_errors: true
typedef OnEggFn = function void(int a);

native void Do(OnEggFn egg);

public main()
{
  Do(Crab);
}

void Crab(int a)
{
}

