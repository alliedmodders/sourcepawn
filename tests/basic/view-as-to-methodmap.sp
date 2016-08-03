native int printnum(int n);

methodmap Egg {
  property int Value {
    public get() {
      return view_as<int>(this);
    }
  }
}

public main()
{
  printnum(
    view_as<Egg>(2).Value
  );
}
