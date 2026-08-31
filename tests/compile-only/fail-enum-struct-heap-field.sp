#include <shell>

class Item {
    int id;
}

enum struct Container {
    Item item;
    Item items[3];
}

public void main() {
    Container c;
    printnum(c.items[0].id);
}
