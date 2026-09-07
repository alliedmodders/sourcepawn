// returnCode: 0
#include <shell>

enum struct SourceLoc {
  char filename[256];
  int line;
  char funcname[256];
}

typedef SourceLocCallback = function void (const SourceLoc loc);
native void call_with_flat_array(SourceLocCallback fn, const SourceLoc loc, int length);

void Func(const SourceLoc loc) {
  printf("loc [%s::%d::%s]\n", loc.filename, loc.line, loc.funcname);
}

public void main() {
  SourceLoc loc = {"filename", 123, "funcname"};
  call_with_flat_array(Func, loc, sizeof(loc));
}
