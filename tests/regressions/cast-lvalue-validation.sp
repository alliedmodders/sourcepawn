#include <shell>

bool g_bArray[10];

stock void Process(int val) {
    printnum(val);
}

public void main() {
    int client = 1;
    g_bArray[client] = true;
    Process(view_as<bool>(g_bArray[client]));
}
