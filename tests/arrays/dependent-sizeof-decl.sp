//#include <shell>

native void printnum(int x);

#define MAX_CHOICE_LABEL_LENGTH 16

new String:wirecolours[9][MAX_CHOICE_LABEL_LENGTH]
new wiremap[sizeof(wirecolours)]

public main() {
  printnum(sizeof(wiremap))
}
