// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
//  Pawn compiler - Error message system
//  In fact a very simple system, using only 'panic mode'.
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

struct stringlist {
  stringlist *next;
  union {
    char *line;
    stringlist *tail;
  };
};

char* duplicatestring(const char* sourcestring);
stringpair *insert_alias(const char *name,const char *alias);
stringpair *find_alias(char *name);
int lookup_alias(char *target,const char *name);
void delete_aliastable(void);
stringlist *insert_path(char *path);
char *get_path(int index);
void delete_pathtable(void);
stringpair *insert_subst(const char *pattern,const char *substitution,int prefixlen);
int get_subst(int index,char **pattern,char **substitution);
stringpair *find_subst(char *name,int length);
int delete_subst(char *name,int length);
void delete_substtable(void);
stringlist *insert_sourcefile(char *string);
char *get_sourcefile(int index);
void delete_sourcefiletable(void);
stringlist *insert_inputfile(char *string);
char *get_inputfile(int index);
void delete_inputfiletable(void);
stringlist *insert_docstring(char *string);
char *get_docstring(int index);
void delete_docstring(int index);
void delete_docstringtable(void);
stringlist *insert_autolist(const char *string);
char *get_autolist(int index);
void delete_autolisttable(void);
stringlist *insert_dbgfile(const char *filename);
stringlist *insert_dbgline(int linenr);
stringlist *insert_dbgsymbol(symbol *sym);
char *get_dbgstring(int index);
void delete_dbgstringtable(void);
stringlist *get_dbgstrings();
void delete_stringtable(stringlist *root);
