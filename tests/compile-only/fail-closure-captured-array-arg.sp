typedef Callback = () -> int;

Callback GetFlatArrayCallback(int[] arr) {
    return function () -> int {
        return arr[0];
    };
}

public void main() {
    int arr1[] = {1, 2, 3};
    GetFlatArrayCallback(arr1);
}
