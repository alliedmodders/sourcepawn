#include <assert.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include "memfile.h"
#include "osdefs.h"
#if defined __linux__ || defined DARWIN
#    include <unistd.h>
#elif defined WIN32
#    include <io.h>
#endif
#include "sc.h"

int
main(int argc, char* argv[])
{
    return pc_compile(argc, argv);
}
