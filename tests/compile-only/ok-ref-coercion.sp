// warnings_are_errors: true
void ModifyInt(int& val) {
    val = 10;
}
void CallWithRef(int& actual) {
    ModifyInt(actual);
}
public void main() {
    int x = 5;
    CallWithRef(x);
}
