// warnings_are_errors: true

void PassRef(int& x) {
    int y = x;
    PassRef(x);
    PassRef(y);
}

void PassAnyArray(any[] x) {
    #pragma unused x
}

void Varargs(any...) {}

public void main() {
    int x;
    PassRef(x);

    char str[10];
    int arr[10];

    PassAnyArray(arr);   // legal, int[] can coerce to any[]

    Varargs(str, arr, x); // legal, varargs accepts anything
}
