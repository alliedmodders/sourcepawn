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
#pragma once

struct memfile_t;

void* pc_opensrc(char* filename); /* reading only */
void* pc_createsrc(char* filename);
void pc_closesrc(void* handle); /* never delete */
char* pc_readsrc(void* handle, unsigned char* target, int maxchars);
int pc_writesrc(void* handle, unsigned char* source);
void* pc_getpossrc(void* handle);
void pc_resetsrc(void* handle, void* position); /* reset to a position marked earlier */
int pc_eofsrc(void* handle);

memfile_t* pc_openasm(char* filename); /* read/write */
void pc_closeasm(memfile_t* handle, int deletefile);
void pc_resetasm(memfile_t* handle);
int pc_writeasm(memfile_t* handle, const char* str);
char* pc_readasm(memfile_t* handle, char* target, int maxchars);

int pc_printf(const char* message, ...);
