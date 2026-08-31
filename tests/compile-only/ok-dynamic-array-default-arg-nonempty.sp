native void MyNative(int[] ids = {1, 2}, int length = 2);

public void OnPluginStart() {
  MyNative();
}
