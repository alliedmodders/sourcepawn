#include <shell>

class Player {
    int id;
}

typedef GetFn = () -> int;

GetFn BuildMultidimCapture() {
    Player arr[2][3];
    arr[0][0] = new Player();
    arr[0][0].id = 42;
    return function () -> int {
        return arr[0][0].id;
    };
}

public void main() {
    let cb = BuildMultidimCapture();
    printnum(cb());
}
