#include <shell>

public main()
{
    TestStruct a = {1, 2};
    TestStruct b = {3, 4};
    print_test_struct(add_test_structs(a, b));
}
