// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#include "memfile.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "osdefs.h"

memfile_t *memfile_creat(const char *name, size_t init)
{
	memfile_t mf;
	memfile_t *pmf;

	mf.size = init;
	mf.base = (char *)malloc(init);
	mf.usedoffs = 0;
	if (!mf.base)
	{
		return NULL;
	}

	mf.offs = 0;
	mf._static = 0;

	pmf = (memfile_t *)malloc(sizeof(memfile_t));
	memcpy(pmf, &mf, sizeof(memfile_t));

	pmf->name = strdup(name);

	return pmf;
}

void memfile_destroy(memfile_t *mf)
{
	if (!mf)
	{
		return;
	}

	if (!mf->_static)
	{
		free(mf->name);
		free(mf->base);
		free(mf);
	}
}

long memfile_seek(memfile_t *mf, long offset, int whence)
{
  assert(mf!=NULL);
  if (mf->usedoffs == 0)
    return 0L;          /* early exit: not a single byte in the file */

  /* find the size of the memory file */
  long length = mf->usedoffs;

  /* convert the offset to an absolute position */
  switch (whence) {
  case SEEK_SET:
    break;
  case SEEK_CUR:
    offset+=mf->offs;
    break;
  case SEEK_END:
    assert(offset<=0);
    offset+=length;
    break;
  } /* switch */

  /* clamp to the file length limit */
  if (offset<0)
    offset=0;
  else if (offset>length)
    offset=length;

  /* set new position and return it */
	mf->offs = offset;
  return offset;
}

long memfile_tell(memfile_t *mf)
{
	return mf->offs;
}

size_t memfile_read(memfile_t *mf, void *buffer, size_t maxsize)
{
	if (!maxsize || mf->offs >= mf->usedoffs)
	{
		return 0;
	}

	if (mf->usedoffs - mf->offs < (long)maxsize)
	{
		maxsize = mf->usedoffs - mf->offs;
		if (!maxsize)
		{
			return 0;
		}
	}

	memcpy(buffer, mf->base + mf->offs, maxsize);

	mf->offs += maxsize;

	return maxsize;
}

int memfile_write(memfile_t *mf, const void *buffer, size_t size)
{
	if (mf->offs + size > mf->size)
	{
		size_t newsize = (mf->size + size) * 2;
		if (mf->_static)
		{
			char *oldbase = mf->base;
			mf->base = (char *)malloc(newsize);
			if (!mf->base)
			{
				return 0;
			}
			memcpy(mf->base, oldbase, mf->size);
		} else {
			mf->base = (char *)realloc(mf->base, newsize);
			if (!mf->base)
			{
				return 0;
			}
		}
		mf->_static = 0;
		mf->size = newsize;
	}
	memcpy(mf->base + mf->offs, buffer, size);
	mf->offs += size;

	if (mf->offs > mf->usedoffs)
	{
		mf->usedoffs = mf->offs;
	}

	return 1;
}

void memfile_reset(memfile_t *mf)
{
  mf->usedoffs = 0;
  mf->offs = 0;
}

char *memfile_gets(memfile_t* mf, char* string, unsigned int size)
{
  assert(mf!=NULL);

  unsigned int read = memfile_read(mf, string, size);
  if (read==0)
    return NULL;
  long seek=0L;

  /* make sure that the string is zero-terminated */
  assert(read<=size);
  if (read<size) {
    string[read]='\0';
  } else {
    string[size-1]='\0';
    seek=-1;            /* undo reading the character that gets overwritten */
  } /* if */

  /* find the first '\n' */
  char* ptr=strchr(string,'\n');
  if (ptr!=NULL) {
    *(ptr+1)='\0';
    seek=(long)(ptr-string)+1-(long)read;
  } /* if */

  /* undo over-read */
  assert(seek<=0);      /* should seek backward only */
  if (seek!=0)
    memfile_seek(mf, seek, SEEK_CUR);

  return string;
}
