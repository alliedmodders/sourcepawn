#include <shell>

char[] gBuf = null;

void clobber(const char[] s) {
    // Drop the only other reference to the base array while |s| is live.
    gBuf = null;
    printnum(s[0]);
}

public void main() {
    gBuf = new char[16];
    gBuf[0] = 'A';
    gBuf[1] = 0;
    clobber(gBuf[0]);
}
