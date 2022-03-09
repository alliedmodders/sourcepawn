#include <shell>

public main() {
    char a1[] = "\u02af";
    char a2[] = "\u02AF";
    char a3[] = "\U000002af";

    assert_eq(sizeof(a1), 3);
    assert_eq(sizeof(a2), 3);
    assert_eq(sizeof(a3), 3);
    assert_eq(a1[0], a2[0]);
    assert_eq(a1[1], a2[1]);
    assert_eq(a1[2], a2[2]);
    assert_eq(a3[0], a2[0]);
    assert_eq(a3[1], a2[1]);
    assert_eq(a3[2], a2[2]);
    printnum(a1[0]);
    printnum(a1[1]);
    printnum(a1[2]);
}
