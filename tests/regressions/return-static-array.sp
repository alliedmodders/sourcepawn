native void print(const char[] x);
#define MAX_LEN 32

public void main() {
	print(FooterGet());
}

char[] FooterGet()
{
	static char buffer[MAX_LEN] = "asdada";
	
	return buffer;
}
