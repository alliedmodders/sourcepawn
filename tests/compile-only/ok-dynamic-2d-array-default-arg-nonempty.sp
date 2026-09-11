native void MyNative(int[][] ids = {{1, 2}}, int length = 1);

public void OnPluginStart() {
  MyNative();
}
