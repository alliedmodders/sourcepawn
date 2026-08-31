#include <shell>

// Closure mutates the captured array through its handle. The outer function's
// local variable references the same SpArray, so the closure's mutations
// reach the underlying storage.

class Player {
    int id;
}

typedef SumFn = () -> int;

SumFn MakeMutator() {
    Player arr[3];
    arr[0] = new Player();
    arr[1] = new Player();
    arr[2] = new Player();
    arr[0].id = 1;
    arr[1].id = 2;
    arr[2].id = 3;
    return function () -> int {
        arr[0].id = 100;
        arr[1].id = 200;
        arr[2].id = 300;
        return arr[0].id + arr[1].id + arr[2].id;
    };
}

public void main() {
    let mutate = MakeMutator();
    printnum(mutate()); // expects 600: closure mutates then reads
}
