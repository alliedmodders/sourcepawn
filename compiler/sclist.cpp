/*  Pawn compiler  - maintenance of various lists
 *
 *  o  Name list (aliases)
 *  o  Include path list
 *  o  Macro definitions (text substitutions)
 *  o  Documentation tags and automatic listings
 *  o  Debug strings
 *
 *  Copyright (c) ITB CompuPhase, 2001-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "sc.h"
#include "scvars.h"
#include "lstring.h"
#include "errors.h"
#include "sclist.h"
#include "sp_symhash.h"
#include <amtl/am-hashmap.h>
#include <amtl/am-string.h>

#if defined FORTIFY
  #include <alloc/fortify.h>
#endif

static bool sAliasTableInitialized;
static ke::HashMap<sp::CharsAndLength, ke::AString, KeywordTablePolicy> sAliases;

static stringpair *insert_stringpair(stringpair *root,const char *first,const char *second,int matchlength)
{
  stringpair *cur,*pred;

  assert(root!=NULL);
  assert(first!=NULL);
  assert(second!=NULL);
  /* create a new node, and check whether all is okay */
  if ((cur=(stringpair*)malloc(sizeof(stringpair)))==NULL)
    return NULL;
  cur->first=strdup(first);
  cur->second=strdup(second);
  cur->matchlength=matchlength;
  cur->documentation=NULL;
  if (cur->first==NULL || cur->second==NULL) {
    if (cur->first!=NULL)
      free(cur->first);
    if (cur->second!=NULL)
      free(cur->second);
    free(cur);
    return NULL;
  } /* if */
  /* link the node to the tree, find the position */
  for (pred=root; pred->next!=NULL && strcmp(pred->next->first,first)<0; pred=pred->next)
    /* nothing */;
  cur->next=pred->next;
  pred->next=cur;
  return cur;
}

static void delete_stringpairtable(stringpair *root)
{
  stringpair *cur, *next;

  assert(root!=NULL);
  cur=root->next;
  while (cur!=NULL) {
    next=cur->next;
    assert(cur->first!=NULL);
    assert(cur->second!=NULL);
    free(cur->first);
    free(cur->second);
    free(cur);
    cur=next;
  } /* while */
  memset(root,0,sizeof(stringpair));
}

static stringpair *find_stringpair(stringpair *cur,const char *first,int matchlength)
{
  int result=0;

  assert(matchlength>0);  /* the function cannot handle zero-length comparison */
  assert(first!=NULL);
  while (cur!=NULL && result<=0) {
    result=(int)*cur->first - (int)*first;
    if (result==0 && matchlength==cur->matchlength) {
      result=strncmp(cur->first,first,matchlength);
      if (result==0)
        return cur;
    } /* if */
    cur=cur->next;
  } /* while */
  return NULL;
}

static int delete_stringpair(stringpair *root,stringpair *item)
{
  stringpair *cur;

  assert(root!=NULL);
  cur=root;
  while (cur->next!=NULL) {
    if (cur->next==item) {
      cur->next=item->next;     /* unlink from list */
      assert(item->first!=NULL);
      assert(item->second!=NULL);
      free(item->first);
      free(item->second);
      free(item->documentation);
      free(item);
      return TRUE;
    } /* if */
    cur=cur->next;
  } /* while */
  return FALSE;
}

/* ----- string list functions ----------------------------------- */
static stringlist *insert_string(stringlist *root,const char *string)
{
  stringlist *cur;

  assert(string!=NULL);
  if ((cur=(stringlist*)malloc(sizeof(stringlist)))==NULL)
    error(103);       /* insufficient memory (fatal error) */
  if ((cur->line=strdup(string))==NULL)
    error(103);       /* insufficient memory (fatal error) */
  cur->next=NULL;
  if (root->tail)
    root->tail->next=cur;
  else
    root->next=cur;
  root->tail=cur;
  return cur;
}

static char *get_string(stringlist *root,int index)
{
  stringlist *cur;

  assert(root!=NULL);
  cur=root->next;
  while (cur!=NULL && index-->0)
    cur=cur->next;
  if (cur!=NULL) {
    assert(cur->line!=NULL);
    return cur->line;
  } /* if */
  return NULL;
}

void delete_stringtable(stringlist *root)
{
  stringlist *cur,*next;

  assert(root!=NULL);
  cur=root->next;
  while (cur!=NULL) {
    next=cur->next;
    assert(cur->line!=NULL);
    free(cur->line);
    free(cur);
    cur=next;
  } /* while */
  memset(root,0,sizeof(stringlist));
}

void insert_alias(const char *name, const char *alias)
{
  if (!sAliasTableInitialized) {
    sAliases.init(128);
    sAliasTableInitialized = true;
  }

  sp::CharsAndLength key(name, strlen(name));
  auto p = sAliases.findForAdd(key);
  if (p.found())
    p->value = alias;
  else
    sAliases.add(p, key, alias);
}

bool lookup_alias(char *target,const char *name)
{
  if (!sAliasTableInitialized)
    return false;

  sp::CharsAndLength key(name, strlen(name));
  auto p = sAliases.find(key);
  if (!p.found())
    return false;
  ke::SafeStrcpy(target, sNAMEMAX + 1, p->value.chars());
  return true;
}

void delete_aliastable(void)
{
  if (sAliasTableInitialized)
    sAliases.clear();
}

/* ----- include paths list -------------------------------------- */
static stringlist includepaths;  /* directory list for include files */

stringlist *insert_path(char *path)
{
  return insert_string(&includepaths,path);
}

char *get_path(int index)
{
  return get_string(&includepaths,index);
}

void delete_pathtable(void)
{
  delete_stringtable(&includepaths);
  assert(includepaths.next==NULL);
}


/* ----- text substitution patterns ------------------------------ */
static stringpair substpair = { NULL, NULL, NULL};  /* list of substitution pairs */

static stringpair *substindex['z'-PUBLIC_CHAR+1]; /* quick index to first character */
static void adjustindex(char c)
{
  stringpair *cur;
  assert((c>='A' && c<='Z') || (c>='a' && c<='z') || c=='_' || c==PUBLIC_CHAR);
  assert(PUBLIC_CHAR<'A' && 'A'<'_' && '_'<'z');

  for (cur=substpair.next; cur!=NULL && cur->first[0]!=c; cur=cur->next)
    /* nothing */;
  substindex[(int)c-PUBLIC_CHAR]=cur;
}

void insert_subst(const char *pattern,const char *substitution,int prefixlen)
{
  stringpair *cur;

  assert(pattern!=NULL);
  assert(substitution!=NULL);
  if ((cur=insert_stringpair(&substpair,pattern,substitution,prefixlen))==NULL)
    error(103);       /* insufficient memory (fatal error) */
  adjustindex(*pattern);

  if (pc_deprecate.length() > 0) {
	  assert(cur!=NULL);
	  cur->flags|=flgDEPRECATED;
	  if (sc_status==statWRITE) {
      if (cur->documentation)
        free(cur->documentation);
      cur->documentation = strdup(pc_deprecate.chars());
    }
    pc_deprecate = "";
  } else {
	  cur->flags = 0;
	  cur->documentation = NULL;
  } /* if */
}

bool find_subst(const char *name, int length, macro_t* macro)
{
  stringpair *item;
  assert(name!=NULL);
  assert(length>0);
  assert((*name>='A' && *name<='Z') || (*name>='a' && *name<='z') || *name=='_' || *name==PUBLIC_CHAR);
  item=substindex[(int)*name-PUBLIC_CHAR];
  if (item!=NULL)
    item=find_stringpair(item,name,length);

  if (!item)
    return false;

  if (item && (item->flags & flgDEPRECATED) != 0)
  {
    static char macro[128];
    const char *msg = (item->documentation != NULL) ? item->documentation : "";
    strlcpy(macro, item->first, sizeof(macro));

    /* If macro contains an opening parentheses and a percent sign, then assume that
     * it takes arguments and remove them from the warning message.
     */
    char *rem;
    if ((rem = strchr(macro, '(')) != NULL && strchr(macro, '%') > rem)
    {
      *rem = '\0';
    }

    error(234, macro, msg);  /* deprecated (macro/constant) */
  }

  if (macro) {
    macro->first = item->first;
    macro->second = item->second;
  }
  return true;
}

int delete_subst(const char* name, int length)
{
  stringpair *item;
  assert(name!=NULL);
  assert(length>0);
  assert((*name>='A' && *name<='Z') || (*name>='a' && *name<='z') || *name=='_' || *name==PUBLIC_CHAR);
  item=substindex[(int)*name-PUBLIC_CHAR];
  if (item!=NULL)
    item=find_stringpair(item,name,length);
  if (item==NULL)
    return FALSE;
  if (item->documentation)
  {
    free(item->documentation);
    item->documentation=NULL;
  }
  delete_stringpair(&substpair,item);
  adjustindex(*name);
  return TRUE;
}

void delete_substtable(void)
{
  size_t i;
  delete_stringpairtable(&substpair);
  for (i=0; i<sizeof substindex/sizeof substindex[0]; i++)
    substindex[i]=NULL;
}


/* ----- input file list (explicit files) ------------------------ */
static stringlist sourcefiles;

stringlist *insert_sourcefile(char *string)
{
  return insert_string(&sourcefiles,string);
}

char *get_sourcefile(int index)
{
  return get_string(&sourcefiles,index);
}

void delete_sourcefiletable(void)
{
  delete_stringtable(&sourcefiles);
  assert(sourcefiles.next==NULL);
}


/* ----- parsed file list (explicit + included files) ------------ */
static stringlist inputfiles;

stringlist *insert_inputfile(char *string)
{
  if (sc_status!=statFIRST)
    return insert_string(&inputfiles,string);
  return NULL;
}

char *get_inputfile(int index)
{
  return get_string(&inputfiles,index);
}

void delete_inputfiletable(void)
{
  delete_stringtable(&inputfiles);
  assert(inputfiles.next==NULL);
}



/* ----- autolisting --------------------------------------------- */
static stringlist autolist;

stringlist *insert_autolist(const char *string)
{
  return insert_string(&autolist,string);
}

char *get_autolist(int index)
{
  return get_string(&autolist,index);
}

void delete_autolisttable(void)
{
  delete_stringtable(&autolist);
  assert(autolist.next==NULL);
}


/* ----- debug information --------------------------------------- */

#define PRIdC  "d"
#define PRIxC  "x"

static stringlist dbgstrings;

stringlist *insert_dbgfile(const char *filename)
{

  if (sc_status==statWRITE && (sc_debug & sSYMBOLIC)!=0) {
    char string[_MAX_PATH+40];
    assert(filename!=NULL);
    assert(strlen(filename)+40<sizeof string);
    sprintf(string,"F:%" PRIxC " %s",code_idx,filename);
    return insert_string(&dbgstrings,string);
  } /* if */
  return NULL;
}

stringlist *insert_dbgline(int linenr)
{
  if (sc_status==statWRITE && (sc_debug & sSYMBOLIC)!=0) {
    char string[40];
    if (linenr>0)
      linenr--;         /* line numbers are zero-based in the debug information */
    sprintf(string,"L:%" PRIxC " %x",code_idx,linenr);
    return insert_string(&dbgstrings,string);
  } /* if */
  return NULL;
}

stringlist *insert_dbgsymbol(symbol *sym)
{
  if (sc_status==statWRITE && (sc_debug & sSYMBOLIC)!=0) {
    char string[2*sNAMEMAX+128];
    char symname[2*sNAMEMAX+16];

    funcdisplayname(symname,sym->name());
    /* address tag:name codestart codeend ident vclass [tag:dim ...] */
    assert(sym->ident != iFUNCTN);
    sprintf(string,"S:%" PRIxC " %x:%s %" PRIxC " %" PRIxC " %x %x %x",
            sym->addr(),sym->tag,symname,sym->codeaddr,code_idx,sym->ident,sym->vclass,sym->usage);
    if (sym->ident==iARRAY || sym->ident==iREFARRAY) {
      #if !defined NDEBUG
        int count=sym->dim.array.level;
      #endif
      symbol *sub;
      strcat(string," [ ");
      for (sub=sym; sub!=NULL; sub=sub->array_child()) {
        assert(sub->dim.array.level==count--);
        sprintf(string+strlen(string),"%x:%x ",sub->x.tags.index,sub->dim.array.length);
      } /* for */
      strcat(string,"]");
    } /* if */

    if (curfunc) {
      if (!curfunc->function()->dbgstrs)
        curfunc->function()->dbgstrs = (stringlist*)calloc(1, sizeof(stringlist));
      return insert_string(curfunc->function()->dbgstrs, string);
    }
    return insert_string(&dbgstrings, string);
  } /* if */
  return NULL;
}

stringlist *get_dbgstrings()
{
  return &dbgstrings;
}

char *get_dbgstring(int index)
{
  return get_string(&dbgstrings,index);
}

void delete_dbgstringtable(void)
{
  delete_stringtable(&dbgstrings);
  assert(dbgstrings.next==NULL);
}
