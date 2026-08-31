native void MyNative(int[] ids = {});

public void OnPluginStart() {
  int ids[5] = {};
  MyNative(ids);
}
