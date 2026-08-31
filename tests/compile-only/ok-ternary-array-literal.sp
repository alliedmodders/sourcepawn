native void test_func(const int color[4]);

public void main() {
  int team = 2;
  test_func(team == 2 ? { 255, 19, 19, 255 } : { 19, 19, 255, 255 });
}
