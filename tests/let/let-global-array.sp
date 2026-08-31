#include <shell>

char global_array[10] = "global";

public void main() {
    let x = global_array;
    printf("%s\n", x);
}
