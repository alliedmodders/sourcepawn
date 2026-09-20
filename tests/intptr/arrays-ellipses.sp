#include <shell>

intptr g[4] = {7, ...};

public main() {
    intptr a[4] = {7, ...};
    printnumptr(a[0]);
    printnumptr(a[1]);
    printnumptr(a[2]);
    printnumptr(a[3]);

    intptr n[3] = {-5, ...};
    printnumptr(n[0]);
    printnumptr(n[1]);
    printnumptr(n[2]);

    printnumptr(g[0]);
    printnumptr(g[3]);
}
