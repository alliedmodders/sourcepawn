#include <shell>

class Player {
    int index;
    char[] name;
    float health;

    void print() {
        printnum(this.index);
        print(this.name);
        print("\n");
        printfloat(this.health);
    }
}

public void main() {
    let p = new Player();
    p.index = 10;
    p.name = "yams";
    p.health = 100.0;
    p.print();
}
