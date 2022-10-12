#include <shell>

stock float operator=(int value) {
    return float(value);
}

public void main() {
    float f = 1; // SM 1.11+: warning 213: tag mismatch (expected "float", got "int")
    printfloat(f);
}
