// defines: ["A=10"]

#if defined A
#define B 20
#endif

public int DefinesFailTest() {
	return A + B;
}
