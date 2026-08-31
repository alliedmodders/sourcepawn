#include <shell>

class Config {
    int timeout;
    float scale;

    Config(int timeout = 30, float scale = 1.0) {
        this.timeout = timeout;
        this.scale = scale;
    }

    void Print() {
        printnum(this.timeout);
        printfloat(this.scale);
    }
}

public void main() {
    Config a = new Config();
    a.Print();

    Config b = new Config(60);
    b.Print();

    Config c = new Config(10, 2.5);
    c.Print();
}
