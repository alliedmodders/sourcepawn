enum struct Whatever {
	int x;
	int test() { return 0; }
}

public void main() {
	Whatever w;
	int huh = w.test;
}
