native void printnum64(int64 a);

int64 blah[2] = {
    1111111111111111,
    2222222222222222
};

public void main() {
    load_dynamic_i64(blah, 1);
    int64 val = 9999999999999999;
    store_dynamic_i64(blah, 0, val);
    printnum64(blah[0]);
    printnum64(blah[1]);
}

void load_dynamic_i64(int64[] arr, int index) {
    printnum64(arr[index]);
}

void store_dynamic_i64(int64[] arr, int index, int64 value) {
    arr[index] = value;
}
