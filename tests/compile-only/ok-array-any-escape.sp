native void printnum(int n);

enum Tag { T };

void Erased(any[] arr, int n) {
    for (int i = 0; i < n; i++)
        printnum(view_as<int>(arr[i]));
}

void FromAny(any[] arr) {
    printnum(view_as<int>(arr[0]));
}

void ShowTag(Tag arr[3]) {}
void ShowChars(const char[] s) {}

public void main() {
    int a[3] = { 1, 2, 3 };
    Tag t[3] = { T, T, T };
    char buf[6] = "hi";

    Erased(a, 3);
    Erased(t, 3);
    FromAny(a);
    FromAny(t);
    ShowTag(t);
    ShowTag(view_as<Tag>(a));
    ShowChars(buf);
    ShowChars("hi");
}
