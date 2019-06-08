// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <sp_vm_base.h>
#include <sp_vm_api.h>
#include <stdlib.h>
#include <stdarg.h>
#include <amtl/am-cxx.h>

#if defined __linux__ || defined __APPLE__
# if !defined(_GLIBCXX_USE_NOEXCEPT)
#  define _GLIBCXX_USE_NOEXCEPT
# endif
extern "C" void __cxa_pure_virtual(void)
{
}

void* operator new(size_t size)
{
	return malloc(size);
}

void* operator new[](size_t size) 
{
	return malloc(size);
}

void operator delete(void* ptr) _GLIBCXX_USE_NOEXCEPT
{
	free(ptr);
}

void operator delete[](void * ptr) _GLIBCXX_USE_NOEXCEPT
{
	free(ptr);
}
#endif
