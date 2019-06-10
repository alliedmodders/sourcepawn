/*
 * Things needed to compile under linux.
 *
 * Should be reworked totally to use GNU's 'configure'
 */
#ifndef SCLINUX_H
#define SCLINUX_H

/*
 * WinWorld wants '\'. Unices do not.
 */
#define DIRECTORY_SEP_CHAR '/'
#define DIRECTORY_SEP_STR "/"

/*
 * SC assumes that a computer is Little Endian unless told otherwise. It uses
 * (and defines) the macros BYTE_ORDER and BIG_ENDIAN.
 * For Linux, we must overrule these settings with those defined in glibc.
 */
#if !defined __BYTE_ORDER
#    if defined EMSCRIPTEN
#        include <endian.h>
#    else
#        include <stdlib.h>
#    endif
#endif

#if defined __OpenBSD__ || defined __FreeBSD__ || defined __APPLE__
#    define __BYTE_ORDER BYTE_ORDER
#    define __LITTLE_ENDIAN LITTLE_ENDIAN
#    define __BIG_ENDIAN BIG_ENDIAN
#endif

#if !defined __BYTE_ORDER
#    error "Can't figure computer byte order (__BYTE_ORDER macro not found)"
#endif

#endif /* SCLINUX_H */
