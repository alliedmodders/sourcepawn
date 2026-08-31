#include <shell>

class Counter {
    int value;

    property int Count {
        get() {
            return this.value;
        }
        set(int v) {
            this.value = v;
        }
    }

    property float Ratio {
        get() {
            return float(this.value) * 1.5;
        }
    }

    void Increment(int amount) {
        this.value += amount;
    }
}

public void main() {
    Counter c = new Counter();

    c.value = 10;
    printnum(c.Count);

    c.Count = 20;
    printnum(c.value);

    c.Increment(5);
    printnum(c.Count);

    printfloat(c.Ratio);

    c.value = 100;
    printfloat(c.Ratio);
}
