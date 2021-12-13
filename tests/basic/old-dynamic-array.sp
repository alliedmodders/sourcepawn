#include <shell>

public void main() {
	f1(3);
	f2(5);
	f3(9);
}

void f1(int i) {
	int[] arr1 = new int[i];
	arr1[1] = 1;
	printnum(arr1[1]);
}

void f2(int i) {
	new arr2[i];
	arr2[4] = 2;
	printnum(arr2[4]);
}

void f3(int i) {
	decl arr3[i];
	arr3[8] = 3;
	printnum(arr3[8]);
}
