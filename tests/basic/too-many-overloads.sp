#include <shell>

enum Address {};

stock Address operator*(Address a, int b) { return a; }
stock Address operator*(int a, Address b) { return b; }

public void main()
{
    printfloat(2.0 * -3);
}
