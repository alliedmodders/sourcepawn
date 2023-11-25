#include <shell>

public main() {
    Blah();
}

stock Blah() {
    File_AddToDownloadsTable()
}

char _smlib_empty_twodimstring_array[][] = { { '1' } };
stock File_AddToDownloadsTable(const String:ignoreExts[][]=_smlib_empty_twodimstring_array) {
    printnum(ignoreExts[0][0]);
}
