#include <shell>

public int a = 10;

public void main() {
    int a_loc = find_pubvar("a");
    printnum(a_loc);
    printnum(find_pubvar("b"));
    for (int i = 0; i < get_pubvar_count(); i++) {
        char name[64];
        get_pubvar_name(i, name, sizeof(name));
        print(name);
        print("\n");
    }
}
