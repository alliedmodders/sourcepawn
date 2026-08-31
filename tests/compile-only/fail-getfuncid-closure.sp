// type: compiler-output

#include <shell>

public main() {
    shared int count = 0;

    execute(function () -> void { count++; printnum(count); }, 1);
}
