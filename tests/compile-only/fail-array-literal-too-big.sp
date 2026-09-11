int[] rgb() { int array[3]; return array; }

public void main() {
    int a;
    int array[3];
    array = {0, 0, 0, 0};
    array = a ? {0, 0, 0, 0} : rgb();
}
