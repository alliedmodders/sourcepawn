#define and &&
#define IsClientValid(%1)	( 0 < %1 and %1 <= MaxClients )

int MaxClients;

public main() {
	return IsClientValid(1) and IsClientValid(2);
}
