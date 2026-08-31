#include <shell>

char[] GetString() {
    char egg[10] = "egg";
    return egg;
}

public void main() {
    let egg2 = GetString();
    printf("%s\n", egg2);
}
