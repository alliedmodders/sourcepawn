#include <shell>

class Player {
    int health;
    int armor;

    Player(int startHealth, int startArmor) {
        this.health = startHealth;
        this.armor = startArmor;
    }

    void Print() {
        printnum(this.health);
        printnum(this.armor);
    }
}

public void main() {
    Player p = new Player(100, 50);
    p.Print();
}
