#include <shell>

public int sIgnore = 0;
public float NULL_VECTOR[3] = {1.0, 1.0, 1.0};

public main() {
  SDKHooks_DropWeapon(2, 2);
}

void SDKHooks_DropWeapon(int client, int weapon, const float vecTarget[3]=NULL_VECTOR) {
  printnum(client);
  printnum(weapon);
  printfloat(vecTarget[0]);
}
