#include <shell>

int GetInt() { return 42; }
float GetFloat() { return 1.5; }
bool GetBool() { return false; }

public void main() {
    let x = GetInt();
    let y = GetFloat();
    let z = GetBool();
    printnum(x);
    printfloat(y);
    printnum(z);
}
