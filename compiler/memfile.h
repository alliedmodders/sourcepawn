#ifndef _INCLUDE_MEMFILE_H
#define _INCLUDE_MEMFILE_H

#include <stdlib.h>

#include <memory>

#include <amtl/am-string.h>

struct memfile_t {
    ke::AString name;
    std::unique_ptr<char[]> base;
    long offs;
    long usedoffs;
    size_t size;

    const char* pos() const {
        return base.get() + offs;
    }
    const char* end() const {
        return base.get() + usedoffs;
    }
};

/**
 * Creates a new memory file
 * init is the initial size in bytes
 */
memfile_t* memfile_creat(const char* name, size_t init);

/**
 * Frees the memory associated.
 */
void memfile_destroy(memfile_t* mf);

/**
 * Seeks to a given offset (always from start)
 */
long memfile_seek(memfile_t* mf, long seek, int whence);

/**
 * Writes to a memory buffer (expands as necessary).
 * Returns 1 on success, 0 on failure.
 */
int memfile_write(memfile_t* mf, const void* buffer, size_t size);

/**
 * Reads a number of bytes from a memory buffer.
 * Returns the number of bytes read until the end was hit.
 */
size_t memfile_read(memfile_t* mf, void* buffer, size_t maxsize);

/**
 * Returns the current position from the start.
 */
long memfile_tell(memfile_t* mf);

/**
 * Resets all the states of the memory buffer.
 * (does not actually free or zero memory)
 */
void memfile_reset(memfile_t* mf);

#endif //_INCLUDE_MEMFILE_H
