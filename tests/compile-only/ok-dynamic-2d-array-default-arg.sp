native void MyNative(int[][] ids = {}, int length = 0);

public void OnPluginStart() {
  MyNative();
}
