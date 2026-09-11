#include <shell>

// Regression: uninitialized local arrays were not zero-initialized at their
// point of declaration. Because they live in fixed frame slots, stale data
// from previous iterations of a loop leaked into subsequent executions of
// the declaration.

public void main() {
    for (int i = 0; i < 3; i++) {
        char str[3];
        if (i == 0) {
            str[0] = 'f'; str[1] = 'o'; str[2] = 'o';
        }
        print(str);
    }
    print("\n");

    for (int i = 0; i < 2; i++) {
        int nums[4];
        char buf[7];
        if (i == 0) {
            nums[0] = 1; nums[1] = 2; nums[2] = 3; nums[3] = 4;
            for (int j = 0; j < 7; j++)
                buf[j] = 'x';
        }
        int sum = 0;
        for (int j = 0; j < 4; j++)
            sum += nums[j];
        for (int j = 0; j < 7; j++)
            sum += buf[j];
        printnum(sum);
    }
}
