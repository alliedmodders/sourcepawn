#include <shell>

public void main() {
    char soundNameBuffer[64];

    soundNameBuffer = true ? "ninja_dance_01" : "dance_soldier_03";
    print(soundNameBuffer);
    print("\n");

    soundNameBuffer = false ? "ninja_dance_01" : "dance_soldier_03";
    print(soundNameBuffer);
    print("\n");
}
