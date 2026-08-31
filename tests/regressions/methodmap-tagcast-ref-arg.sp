#include <shell>

methodmap Widget {
};

void SetViaRef(any &value) {
    value = 42;
}

public void main() {
    Widget w;
    SetViaRef(any:w);
    printnum(any:w);
}
