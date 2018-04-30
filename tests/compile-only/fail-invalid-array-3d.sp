native void use(...);

int Array2[][][] = {
 {
  {1, 2, 3},
 }, {
  {8, 9, 1, ...},
 }
};

int Array3[][][] = {
 {
  {1, 2, 3},
  {2, 3},
 }, {
  {4, 5},
 }
};

char Array4[5][] = { };

public main()
{
  use(Array2);
  use(Array3[0][1]);
  use(Array4);
}
