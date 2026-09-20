#include <shell>

int64 g[3] = {view_as<int8>(5), 6, view_as<int16>(7)};
intptr gh[2] = {view_as<int8>(5), 6};
float gf[2] = {view_as<int8>(9), 6};
int64 g64[2] = {view_as<intptr>(5), 6};

public main() {
    int64 a[3] = {view_as<int8>(5), 6, view_as<int16>(7)};
    printnum64(a[0]);
    printnum64(a[1]);
    printnum64(a[2]);

    intptr b[2] = {view_as<int8>(5), 6};
    printnumptr(b[0]);
    printnumptr(b[1]);

    float c[2] = {view_as<int8>(9), 6};
    printfloat(c[0]);
    printfloat(c[1]);

    int64 d[2] = {view_as<intptr>(-3), 6};
    printnum64(d[0]);
    printnum64(d[1]);

    printnum64(g[0]);
    printnum64(g[1]);
    printnum64(g[2]);

    printnumptr(gh[0]);
    printnumptr(gh[1]);

    printfloat(gf[0]);
    printfloat(gf[1]);

    printnum64(g64[0]);
    printnum64(g64[1]);
}
