#include <shell>

char g_names[2][8] = {"one", "two"};

void Show(char buf[8]) {
    print(buf);
    print("\n");
}

void Set(char buf[8]) {
    buf[0] = 'X';
}

void Vec(float v[3]) {
    printfloat(v[1]);
}

void Scale(float v[3]) {
    v[1] *= 2.0;
}

public void main() {
    char names[][8] = {"one", "two"};
    Show(names[1]);
    Set(names[1]);
    Show(names[1]);

    Show(g_names[0]);
    Set(g_names[0]);
    Show(g_names[0]);

    float vecs[2][3] = {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}};
    Vec(vecs[1]);
    Scale(vecs[1]);
    Vec(vecs[1]);
}
