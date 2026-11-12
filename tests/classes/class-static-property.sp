#include <shell>

int gTotal = 0;

class Ledger {
    int items;

    static property int Total {
        public get() {
            return gTotal;
        }
        public set(int v) {
            gTotal = v;
        }
    }

    property int Items {
        get() {
            return this.items;
        }
        set(int v) {
            this.items = v;
        }
    }

    void Add(int amount) {
        Ledger.Total += amount;
        this.items += 1;
    }
}

public void main() {
    Ledger.Total = 100;
    printnum(Ledger.Total);

    Ledger a = new Ledger();
    a.Items = 3;
    a.Add(5);
    printnum(Ledger.Total);
    printnum(a.Items);

    Ledger.Total++;
    printnum(Ledger.Total);
    printnum(--Ledger.Total);

    Ledger.Total -= 10;
    printnum(Ledger.Total);
    printnum(a.Items);
}
