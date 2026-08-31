// warnings_are_errors: true
void TakeInt(int val) {
    #pragma unused val
}
void TakeBool(bool val) {
    #pragma unused val
}

public void main() {
    bool b = true;
    int i = 5;
    TakeInt(b);   // bool to int coercion
    TakeBool(i);  // int to bool coercion
}
