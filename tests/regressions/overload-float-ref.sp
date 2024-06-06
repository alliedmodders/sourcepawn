#include <shell>

void test(float& damage) {
    printf("%f\n", damage);
    int new_damage = 10000;
    damage += float(new_damage);
    printf("%f\n", damage);
}

public main() {
    float damage = 143.75;
    test(damage);
}
