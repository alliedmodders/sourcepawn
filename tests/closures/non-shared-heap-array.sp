#include <shell>

// Non-shared capture of a fixed array of heap items. Single-dim fixed
// arrays are runtime-equivalent to dynamic arrays (heap-allocated SpArray),
// so the closure holds a reference to the same SpArray and sees post-capture
// mutations just like a dynamic-array capture would.

class Player {
    int id;
}

typedef GetFn = () -> int;

GetFn BuildNonSharedCapture() {
    Player arr[3];
    arr[0] = new Player();
    arr[1] = new Player();
    arr[2] = new Player();
    arr[0].id = 10;
    arr[1].id = 20;
    arr[2].id = 30;
    let cb = function () -> int {
        return arr[0].id + arr[1].id + arr[2].id;
    };
    arr[0].id = 100;
    arr[1].id = 200;
    arr[2].id = 300;
    return cb;
}

public void main() {
    let cb = BuildNonSharedCapture();
    printnum(cb()); // expects 600: post-capture mutations visible
}
