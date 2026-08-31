// returnCode: 1
#include <shell>

// Regression: a function whose locals exceed UINT16_MAX cells must raise
// SP_ERROR_STACKLOW during v2 lowering rather than crashing or producing
// undefined behavior. Each int[1024] is exactly 4096 bytes, which stays
// below the 4096-byte flat-array cutoff; 64 of them = 65536 cells, just
// past UINT16_MAX.

public main() {
    int a0[1024]; int a1[1024]; int a2[1024]; int a3[1024];
    int a4[1024]; int a5[1024]; int a6[1024]; int a7[1024];
    int a8[1024]; int a9[1024]; int a10[1024]; int a11[1024];
    int a12[1024]; int a13[1024]; int a14[1024]; int a15[1024];
    int a16[1024]; int a17[1024]; int a18[1024]; int a19[1024];
    int a20[1024]; int a21[1024]; int a22[1024]; int a23[1024];
    int a24[1024]; int a25[1024]; int a26[1024]; int a27[1024];
    int a28[1024]; int a29[1024]; int a30[1024]; int a31[1024];
    int a32[1024]; int a33[1024]; int a34[1024]; int a35[1024];
    int a36[1024]; int a37[1024]; int a38[1024]; int a39[1024];
    int a40[1024]; int a41[1024]; int a42[1024]; int a43[1024];
    int a44[1024]; int a45[1024]; int a46[1024]; int a47[1024];
    int a48[1024]; int a49[1024]; int a50[1024]; int a51[1024];
    int a52[1024]; int a53[1024]; int a54[1024]; int a55[1024];
    int a56[1024]; int a57[1024]; int a58[1024]; int a59[1024];
    int a60[1024]; int a61[1024]; int a62[1024]; int a63[1024];
    a0[0] = 42;
    printnum(a0[0]);
    return 0;
}
