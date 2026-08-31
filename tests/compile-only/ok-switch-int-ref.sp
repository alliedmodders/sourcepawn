// warnings_are_errors: true

void test_switch(int& val) {
    switch (val) {
        case 1: {}
        case 2: {}
    }
}

public void main() {
    int x = 1;
    test_switch(x);
}
