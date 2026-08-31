#include <shell>

class Matrix {
    int data[3];

    void SetRow(int row, int v) {
        this.data[row] = v;
    }

    void PrintRow(int row) {
        printnum(this.data[row]);
    }

    int GetRow(int row) {
        return this.data[row];
    }

    void SetAll(int a, int b, int c) {
        this.data[0] = a;
        this.data[1] = b;
        this.data[2] = c;
    }
}

public void main() {
    Matrix m = new Matrix();
    m.SetAll(10, 20, 30);

    m.PrintRow(0);
    m.PrintRow(1);
    m.PrintRow(2);
}
