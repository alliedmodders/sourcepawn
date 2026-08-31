// returnCode: 1
// Minimal reproduction - JIT does not release local heap items when an
// exception is thrown mid-function. The local array arr is leaked in JIT
// mode but correctly freed in interpreter mode. Run spshell with --leak-check
// to see the LEAK DETECTED report.
public main() {
    int[] arr = new int[5];
    arr[10] = 1;
}
