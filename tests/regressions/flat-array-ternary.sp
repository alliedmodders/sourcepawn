#include <shell>

int g_arr[3] = { 1, 2, 3 };
int m_arr[2][3] = { { 10, 20, 30 }, { 40, 50, 60 } };

public main() {
    int result[3];

    result = true ? g_arr : m_arr[0];
    printnum(result[0]);
    printnum(result[1]);
    printnum(result[2]);

    result = false ? g_arr : m_arr[1];
    printnum(result[0]);
    printnum(result[1]);
    printnum(result[2]);
}
