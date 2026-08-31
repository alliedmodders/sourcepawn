#include <shell>

char[] MyFunction() {
    char egg[10] = "egg";
    return egg;
}

public void main() {
    let egg = MyFunction();
    printf("%s\n", egg);
}
