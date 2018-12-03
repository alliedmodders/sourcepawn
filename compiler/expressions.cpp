/* vim: set ts=8 sts=2 sw=2 tw=99 et: */
/*  Pawn compiler - Recursive descend expresion parser
 *
 *  Copyright (c) ITB CompuPhase, 1997-2005
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
 *      appreciated but is not reeq;quired.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>     /* for _MAX_PATH */
#include <string.h>
#if defined FORTIFY
  #include <alloc/fortify.h>
#endif
#include "sc.h"
#include "scvars.h"
#include "codegen.h"
#include "errors.h"
#include "sctracker.h"
#include "lexer.h"
#include "optimizer.h"
#include "types.h"
#include "expressions.h"
#include <amtl/am-algorithm.h>

static void dropout(int lvalue,void (*testfunc)(int val),int exit1,value *lval);
static cell calc(cell left,void (*oper)(),cell right,char *boolresult);
static void clear_value(value *lval);
static int dbltest(void (*oper)(),value *lval1,value *lval2);
static int commutative(void (*oper)());
static int constant(value *lval);

static char lastsymbol[sNAMEMAX+1]; /* name of last function/variable */

/* Function addresses of binary operators for signed operations */
static void (*op1[17])(void) = {
  os_mult,os_div,os_mod,        /* hier3, index 0 */
  ob_add,ob_sub,                /* hier4, index 3 */
  ob_sal,os_sar,ou_sar,         /* hier5, index 5 */
  ob_and,                       /* hier6, index 8 */
  ob_xor,                       /* hier7, index 9 */
  ob_or,                        /* hier8, index 10 */
  os_le,os_ge,os_lt,os_gt,      /* hier9, index 11 */
  ob_eq,ob_ne,                  /* hier10, index 15 */
};
/* These two functions are defined because the functions inc() and dec() in
 * SC4.C have a different prototype than the other code generation functions.
 * The arrays for user-defined functions use the function pointers for
 * identifying what kind of operation is requested; these functions must all
 * have the same prototype. As inc() and dec() are special cases already, it
 * is simplest to add two "do-nothing" functions.
 */
static void user_inc(void) {}
static void user_dec(void) {}

int check_userop(void (*oper)(void),int tag1,int tag2,int numparam,
                         value *lval,int *resulttag)
{
  static const char *binoperstr[] = {
    "*", "/", "%", "+", "-", "", "", "",
    "", "", "", "<=", ">=", "<", ">", "==", "!="
  };
  static int binoper_savepri[] = { FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                   FALSE, FALSE, FALSE, FALSE, FALSE,
                                   TRUE, TRUE, TRUE, TRUE, FALSE, FALSE
  };
  static const char *unoperstr[] = { "!", "-", "++", "--" };
  static void (*unopers[])(void) = { lneg, neg, user_inc, user_dec };

  char opername[4] = "", symbolname[sNAMEMAX+1];
  size_t i;
  int swapparams,savepri,savealt;
  int paramspassed;
  symbol *sym;

  /* since user-defined operators on untagged operands are forbidden, we have
   * a quick exit.
   */
  assert(numparam==1 || numparam==2);
  if (tag1==0 && (numparam==1 || tag2==0))
    return FALSE;

  savepri=savealt=FALSE;
  /* find the name with the operator */
  if (numparam==2) {
    if (oper==NULL) {
      /* assignment operator: a special case */
      strcpy(opername,"=");
      if (lval!=NULL && (lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR))
        savealt=TRUE;
    } else {
      assert( (sizeof binoperstr / sizeof binoperstr[0]) == (sizeof op1 / sizeof op1[0]) );
      for (i=0; i<sizeof op1 / sizeof op1[0]; i++) {
        if (oper==op1[i]) {
          strcpy(opername,binoperstr[i]);
          savepri=binoper_savepri[i];
          break;
        } /* if */
      } /* for */
    } /* if */
  } else {
    assert(oper!=NULL);
    assert(numparam==1);
    /* try a select group of unary operators */
    assert( (sizeof unoperstr / sizeof unoperstr[0]) == (sizeof unopers / sizeof unopers[0]) );
    if (opername[0]=='\0') {
      for (i=0; i<sizeof unopers / sizeof unopers[0]; i++) {
        if (oper==unopers[i]) {
          strcpy(opername,unoperstr[i]);
          break;
        } /* if */
      } /* for */
    } /* if */
  } /* if */
  /* if not found, quit */
  if (opername[0]=='\0')
    return FALSE;

  /* create a symbol name from the tags and the operator name */
  assert(numparam==1 || numparam==2);
  operator_symname(symbolname,opername,tag1,tag2,numparam,tag2);
  swapparams=FALSE;
  sym=findglb(symbolname);
  if (sym==NULL /*|| (sym->usage & uDEFINE)==0*/) {  /* ??? should not check uDEFINE; first pass clears these bits */
    /* check for commutative operators */
    if (tag1==tag2 || oper==NULL || !commutative(oper))
      return FALSE;             /* not commutative, cannot swap operands */
    /* if arrived here, the operator is commutative and the tags are different,
     * swap tags and try again
     */
    assert(numparam==2);        /* commutative operator must be a binary operator */
    operator_symname(symbolname,opername,tag2,tag1,numparam,tag1);
    swapparams=TRUE;
    sym=findglb(symbolname);
    if (sym==NULL /*|| (sym->usage & uDEFINE)==0*/)
      return FALSE;
  } /* if */

  /* check existance and the proper declaration of this function */
  if ((sym->usage & uMISSING)!=0 || (sym->usage & uPROTOTYPED)==0) {
    char symname[2*sNAMEMAX+16];  /* allow space for user defined operators */
    funcdisplayname(symname,sym->name());
    if ((sym->usage & uMISSING)!=0)
      error(4,symname);           /* function not defined */
    if ((sym->usage & uPROTOTYPED)==0)
      error(71,symname);          /* operator must be declared before use */
  } /* if */

  /* we don't want to use the redefined operator in the function that
   * redefines the operator itself, otherwise the snippet below gives
   * an unexpected recursion:
   *    fixed:operator+(fixed:a, fixed:b)
   *        return a + b
   */
  if (sym==curfunc)
    return FALSE;

  /* for increment and decrement operators, the symbol must first be loaded
   * (and stored back afterwards)
   */
  if (oper==user_inc || oper==user_dec) {
    assert(!savepri);
    assert(lval!=NULL);
    if (lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR)
      pushreg(sPRI);            /* save current address in PRI */
    if (lval->ident!=iACCESSOR)
      rvalue(lval);               /* get the symbol's value in PRI */
  } /* if */

  assert(!savepri || !savealt); /* either one MAY be set, but not both */
  if (savepri) {
    /* the chained comparison operators require that the ALT register is
     * unmodified, so we save it here; actually, we save PRI because the normal
     * instruction sequence (without user operator) swaps PRI and ALT
     */
    pushreg(sPRI);              /* right-hand operand is in PRI */
  } else if (savealt) {
    /* for the assignment operator, ALT may contain an address at which the
     * result must be stored; this address must be preserved accross the
     * call
     */
    assert(lval!=NULL);         /* this was checked earlier */
    assert(lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR); /* checked earlier */
    pushreg(sALT);
  } /* if */

  /* push parameters, call the function */
  paramspassed= (oper==NULL) ? 1 : numparam;
  switch (paramspassed) {
  case 1:
    pushreg(sPRI);
    break;
  case 2:
    /* note that 1) a function expects that the parameters are pushed
     * in reversed order, and 2) the left operand is in the secondary register
     * and the right operand is in the primary register */
    if (swapparams) {
      pushreg(sALT);
      pushreg(sPRI);
    } else {
      pushreg(sPRI);
      pushreg(sALT);
    } /* if */
    break;
  default:
    assert(0);
  } /* switch */
  markexpr(sPARM,NULL,0);       /* mark the end of a sub-expression */
  assert(sym->ident==iFUNCTN);
  ffcall(sym,NULL,paramspassed);
  if (sc_status!=statSKIP)
    markusage(sym,uREAD);       /* do not mark as "used" when this call itself is skipped */
  sideeffect=TRUE;              /* assume functions carry out a side-effect */
  assert(resulttag!=NULL);
  *resulttag=sym->tag;          /* save tag of the called function */

  if (savepri || savealt)
    popreg(sALT);               /* restore the saved PRI/ALT that into ALT */
  if (oper==user_inc || oper==user_dec) {
    assert(lval!=NULL);
    if (lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR)
      popreg(sALT);             /* restore address (in ALT) */
    if (lval->ident!=iACCESSOR) {
      store(lval);                /* store PRI in the symbol */
      moveto1();                  /* make sure PRI is restored on exit */
    }
  } /* if */
  return TRUE;
}

int checktag_string(int tag, value *sym1)
{
  if (sym1->ident == iARRAY || sym1->ident == iREFARRAY)
    return FALSE;
  if ((sym1->tag == pc_tag_string && tag == 0) ||
      (sym1->tag == 0 && tag == pc_tag_string))
  {
    return TRUE;
  }
  return FALSE;
}

int checkval_string(value *sym1, value *sym2)
{
  if (sym1->ident == iARRAY || sym2->ident == iARRAY ||
      sym1->ident == iREFARRAY || sym2->ident == iREFARRAY)
  {
    return FALSE;
  }
  if ((sym1->tag == pc_tag_string && sym2->tag == 0) ||
      (sym1->tag == 0 && sym2->tag == pc_tag_string))
  {
    return TRUE;
  }

  return FALSE;
}

const char *type_to_name(int tag)
{
  if (tag == 0)
    return "int";
  if (tag == sc_rationaltag)
    return "float";
  if (tag == pc_tag_string)
    return "char";
  if (tag == pc_anytag)
    return "any";

  const char *name = pc_tagname(tag);
  if (name)
    return name;

  Type* type = gTypes.find(tag);
  if (type && type->isFunction())
    return "function";
  return "unknown";
}

int matchtag_string(int ident, int tag)
{
  if (ident == iARRAY || ident == iREFARRAY)
    return FALSE;
  return (tag == pc_tag_string) ? TRUE : FALSE;
}

static int obj_typeerror(int id, int tag1, int tag2)
{
  const char *left = pc_tagname(tag1);
  const char *right = pc_tagname(tag2);
  if (!left || strcmp(left, "_") == 0)
    left = "int";
  if (!right || strcmp(right, "_") == 0)
    right = "int";
  error(id, right, left);
  return FALSE;
}

static int matchobjecttags(Type* formal, Type* actual, int flags)
{
  int formaltag = formal->tagid();
  int actualtag = actual->tagid();

  if ((flags & MATCHTAG_COMMUTATIVE) &&
      (formaltag == pc_tag_null_t || formaltag == pc_tag_nullfunc_t))
  {
    // Bypass the check immediately after for non-object coercion.
    ke::Swap(formaltag, actualtag);
    ke::Swap(formal, actual);
  }

  // objects never coerce to non-objects, YET.
  if (formal->isObject() && !actual->isObject())
    return obj_typeerror(132, formaltag, actualtag);

  if (actualtag == pc_tag_nullfunc_t) {
    // All functions are nullable. We use a separate constant for backward
    // compatibility; plugins and extensions check -1, not 0.
    if (formal->isFunction())
      return TRUE;

    error(154, pc_tagname(formaltag));
    return FALSE;
  }

  if (actualtag == pc_tag_null_t) {
    // All objects are nullable.
    if (formal->isObject())
      return TRUE;

    // Some methodmaps are nullable. The nullable property is inherited
    // automatically.
    methodmap_t *map = formal->asMethodmap();
    if (map && map->nullable)
      return TRUE;

    error(148, pc_tagname(formaltag));
    return FALSE;
  }

  if (!formal->isObject() && actual->isObject())
    return obj_typeerror(131, formaltag, actualtag);

  // Every object coerces to "object".
  if (formaltag == pc_tag_object)
    return TRUE;

  if (flags & MATCHTAG_COERCE)
    return obj_typeerror(134, formaltag, actualtag);

  methodmap_t *map = actual->asMethodmap();
  for (; map; map = map->parent) {
    if (map->tag == formaltag)
      return TRUE;
  }

  return obj_typeerror(133, formaltag, actualtag);
}

static int matchreturntag(const functag_t *formal, const functag_t *actual)
{
  if (formal->ret_tag == actual->ret_tag)
    return TRUE;
  if (formal->ret_tag == pc_tag_void) {
    if (actual->ret_tag == 0 && !(actual->usage & uRETVALUE))
      return TRUE;
  }
  return FALSE;
}

static int funcarg_compare(const funcarg_t *formal, const funcarg_t *actual)
{
  // Check type.
  if (actual->ident != formal->ident)
    return FALSE;

  // Check rank.
  if (actual->dimcount != formal->dimcount)
    return FALSE;

  // Check arity.
  for (int i = 0; i < formal->dimcount; i++) {
    if (actual->dims[i] != formal->dims[i])
      return FALSE;
  }

  // Check tags.
  if (actual->tagcount != formal->tagcount)
    return FALSE;
  for (int i = 0; i < formal->tagcount; i++) {
    // Note we invert the order we pass things to matchtag() here. If the
    // typedef specifies base type X, and the function specifies derived
    // type Y, we want this to type since such an assignment is valid.
    // 
    // Most programming languages do not subtype arguments like this. We do
    // it in SourcePawn to preserve compatibility during the Transitional
    // Syntax effort.
    int actual_tag = actual->tags[i];
    int formal_tag = formal->tags[i];
    if (!matchtag(actual_tag, formal_tag, MATCHTAG_SILENT|MATCHTAG_COERCE))
      return FALSE;
  }

  return TRUE;
}

static int functag_compare(const functag_t *formal, const functag_t *actual)
{
  // Check return types.
  if (!matchreturntag(formal, actual))
    return FALSE;

  // Make sure there are no trailing arguments.
  if (actual->argcount > formal->argcount)
    return FALSE;

  // Check arguments.
  for (int i = 0; i < formal->argcount; i++) {
    const funcarg_t *formal_arg = &formal->args[i];

    if (i >= actual->argcount) {
      if (formal_arg->ommittable)
        return TRUE;
      return FALSE;
    }

    const funcarg_t *actual_arg = &actual->args[i];
    if (!funcarg_compare(formal_arg, actual_arg))
      return FALSE;
  }

  return TRUE;
}

static int matchfunctags(Type* formal, Type* actual)
{
  int formaltag = formal->tagid();
  int actualtag = actual->tagid();

  if (formaltag == pc_functag && actual->isFunction())
    return TRUE;

  if (actualtag == pc_tag_nullfunc_t)
    return TRUE;

  if (!actual->isFunction())
    return FALSE;

  functag_t *actualfn = functag_find_intrinsic(actualtag);
  if (!actualfn)
    return FALSE;

  funcenum_t *e = formal->toFunction();
  if (!e)
    return FALSE;

  for (const auto& formalfn : e->entries) {
    if (functag_compare(formalfn.get(), actualfn))
      return TRUE;
  }

  return FALSE;
}

int matchtag(int formaltag, int actualtag, int flags)
{
  if (formaltag == actualtag)
    return TRUE;

  Type* actual = gTypes.find(actualtag);
  Type* formal = gTypes.find(formaltag);
  assert(actual && formal);

  if (formaltag == pc_tag_string && actualtag == 0)
    return TRUE;

  if (formal->isObject() || actual->isObject())
    return matchobjecttags(formal, actual, flags);

  if (actual->isFunction() && !formal->isFunction()) {
    // We're being given a function, but the destination is not a function.
    error(130);
    return FALSE;
  }

  /* if the formal tag is zero and the actual tag is not "fixed", the actual
   * tag is "coerced" to zero
   */
  if ((flags & MATCHTAG_COERCE) &&
      !formaltag &&
      actual && !actual->isFixed())
  {
    return TRUE;
  }

  if (formaltag == pc_anytag || actualtag == pc_anytag)
    return TRUE;

  if (formal->isFunction()) {
    if (!matchfunctags(formal, actual)) {
      error(100);
      return FALSE;
    }
    return TRUE;
  }

  if (flags & (MATCHTAG_COERCE|MATCHTAG_DEDUCE)) {
    // See if the tag has a methodmap associated with it. If so, see if the given
    // tag is anywhere on the inheritance chain.
    if (methodmap_t *map = actual->asMethodmap()) {
      for (; map; map = map->parent) {
        if (map->tag == formaltag)
          return TRUE;
      }
    }
  }
  
  if (!(flags & MATCHTAG_SILENT))
    error(213);
  return FALSE;
}

/*
 *  The AMX pseudo-processor has no direct support for logical (boolean)
 *  operations. These have to be done via comparing and jumping. Since we are
 *  already jumping through the code, we might as well implement an "early
 *  drop-out" evaluation (also called "short-circuit"). This conforms to
 *  standard C:
 *
 *  expr1 || expr2           expr2 will only be evaluated if expr1 is false.
 *  expr1 && expr2           expr2 will only be evaluated if expr1 is true.
 *
 *  expr1 || expr2 && expr3  expr2 will only be evaluated if expr1 is false
 *                           and expr3 will only be evaluated if expr1 is
 *                           false and expr2 is true.
 *
 *  Code generation for the last example proceeds thus:
 *
 *      evaluate expr1
 *      operator || found
 *      jump to "l1" if result of expr1 not equal to 0
 *      evaluate expr2
 *      ->  operator && found; skip to higher level in hierarchy diagram
 *          jump to "l2" if result of expr2 equal to 0
 *          evaluate expr3
 *          jump to "l2" if result of expr3 equal to 0
 *          set expression result to 1 (true)
 *          jump to "l3"
 *      l2: set expression result to 0 (false)
 *      l3:
 *      <-  drop back to previous hierarchy level
 *      jump to "l1" if result of expr2 && expr3 not equal to 0
 *      set expression result to 0 (false)
 *      jump to "l4"
 *  l1: set expression result to 1 (true)
 *  l4:
 *
 */

/*  Skim over terms adjoining || and && operators
 *  dropval   The value of the expression after "dropping out". An "or" drops
 *            out when the left hand is TRUE, so dropval must be 1 on "or"
 *            expressions.
 *  endval    The value of the expression when no expression drops out. In an
 *            "or" expression, this happens when both the left hand and the
 *            right hand are FALSE, so endval must be 0 for "or" expressions.
 */
int
SC3ExpressionParser::skim(int *opstr,void (*testfunc)(int),int dropval,int endval,
                       HierFn hier, value *lval)
{
  int lvalue,hits,droplab,endlab,opidx;
  int allconst,foundop;
  cell constval;
  int index;
  cell cidx;

  stgget(&index,&cidx);         /* mark position in code generator */
  hits=FALSE;                   /* no logical operators "hit" yet */
  allconst=TRUE;                /* assume all values "const" */
  constval=0;
  droplab=0;                    /* to avoid a compiler warning */
  for ( ;; ) {
    lvalue=plnge1(hier,lval);   /* evaluate left expression */

    allconst= allconst && (lval->ident==iCONSTEXPR);
    if (allconst) {
      if (hits) {
        /* one operator was already found */
        if (testfunc==jmp_ne0)
          lval->constval= lval->constval || constval;
        else
          lval->constval= lval->constval && constval;
      } /* if */
      constval=lval->constval;  /* save result accumulated so far */
    } /* if */

    foundop=nextop(&opidx,opstr);
    if ((foundop || hits) && (lval->ident==iARRAY || lval->ident==iREFARRAY))
      error(33, lval->sym ? lval->sym->name() : "-unknown-");  /* array was not indexed in an expression */
    if (foundop) {
      if (!hits) {
        /* this is the first operator in the list */
        hits=TRUE;
        droplab=getlabel();
      } /* if */
      dropout(lvalue,testfunc,droplab,lval);
    } else if (hits) {                       /* no (more) identical operators */
      dropout(lvalue,testfunc,droplab,lval); /* found at least one operator! */
      ldconst(endval,sPRI);
      jumplabel(endlab=getlabel());
      setlabel(droplab);
      ldconst(dropval,sPRI);
      setlabel(endlab);
      lval->sym=NULL;
      lval->tag=pc_addtag("bool");  /* force tag to be "bool" */
      if (allconst) {
        lval->ident=iCONSTEXPR;
        lval->constval=constval;
        stgdel(index,cidx);         /* scratch generated code and calculate */
      } else {
        lval->ident=iEXPRESSION;
        lval->constval=0;
      } /* if */
      return FALSE;
    } else {
      return lvalue;            /* none of the operators in "opstr" were found */
    } /* if */

  } /* while */
}

/*
 *  Reads into the primary register the variable pointed to by lval if
 *  plunging through the hierarchy levels detected an lvalue. Otherwise
 *  if a constant was detected, it is loaded. If there is no constant and
 *  no lvalue, the primary register must already contain the expression
 *  result.
 *
 *  After that, the compare routines "jmp_ne0" or "jmp_eq0" are called, which
 *  compare the primary register against 0, and jump to the "early drop-out"
 *  label "exit1" if the condition is true.
 */
static void dropout(int lvalue,void (*testfunc)(int val),int exit1,value *lval)
{
  if (lvalue)
    rvalue(lval);
  else if (lval->ident==iCONSTEXPR)
    ldconst(lval->constval,sPRI);
  (*testfunc)(exit1);
}

static void checkfunction(value *lval)
{
  symbol *sym=lval->sym;

  if (sym==NULL || (sym->ident!=iFUNCTN))
    return;             /* no known symbol, or not a function result */

  if ((sym->usage & uDEFINE)!=0) {
    /* function is defined, can now check the return value (but make an
     * exception for directly recursive functions)
     */
    if (sym!=curfunc && (sym->usage & uRETVALUE)==0) {
      char symname[2*sNAMEMAX+16];  /* allow space for user defined operators */
      funcdisplayname(symname,sym->name());
      error(209,symname);       /* function should return a value */
    } /* if */
  } else {
    /* function not yet defined, set */
    sym->usage|=uRETVALUE;      /* make sure that a future implementation of
                                 * the function uses "return <value>" */
  } /* if */
}

/*
 *  Plunge to a lower level
 */
int
SC3ExpressionParser::plnge(int *opstr,int opoff,
                        HierFn hier, value *lval,
                        const char *forcetag,int chkbitwise)
{
  int lvalue,opidx;
  int count;
  value lval2 = {0};

  lvalue=plnge1(hier,lval);
  if (nextop(&opidx,opstr)==0)
    return lvalue;              /* no operator in "opstr" found */
  if (lvalue)
    rvalue(lval);
  count=0;
  do {
    if (chkbitwise && count++>0 && bitwise_opercount_!=0)
      error(212);
    opidx+=opoff;               /* add offset to index returned by nextop() */
    plnge2(op1[opidx],hier,lval,&lval2);
    if (op1[opidx]==ob_and || op1[opidx]==ob_or)
      bitwise_opercount_++;
    if (forcetag!=NULL)
      lval->tag=pc_addtag(forcetag);
  } while (nextop(&opidx,opstr)); /* do */
  return FALSE;         /* result of expression is not an lvalue */
}

/*  plnge_rel
 *
 *  Binary plunge to lower level; this is very simular to plnge, but
 *  it has special code generation sequences for chained operations.
 */
int
SC3ExpressionParser::plnge_rel(int *opstr,int opoff,HierFn hier,value *lval)
{
  int lvalue,opidx;
  value lval2={0};
  int count;
  char boolresult;

  /* this function should only be called for relational operators */
  assert(op1[opoff]==os_le);
  lvalue=plnge1(hier,lval);
  if (nextop(&opidx,opstr)==0)
    return lvalue;              /* no operator in "opstr" found */
  if (lvalue)
    rvalue(lval);
  count=0;
  lval->boolresult=TRUE;
  do {
    /* same check as in plnge(), but "chkbitwise" is always TRUE */
    if (count>0 && bitwise_opercount_!=0)
      error(212);
    if (count>0) {
      relop_prefix();
      boolresult=lval->boolresult;
      *lval=lval2;      /* copy right hand expression of the previous iteration */
      lval->boolresult=boolresult;
    } /* if */
    opidx+=opoff;
    plnge2(op1[opidx],hier,lval,&lval2);
    if (count++>0)
      relop_suffix();
  } while (nextop(&opidx,opstr)); /* enddo */
  lval->constval=lval->boolresult;
  lval->tag=pc_addtag("bool");    /* force tag to be "bool" */
  return FALSE;         /* result of expression is not an lvalue */
}

/*  plnge1
 *
 *  Unary plunge to lower level
 *  Called by: skim(), plnge(), plnge2(), plnge_rel(), hier14() and hier13()
 */
int
SC3ExpressionParser::plnge1(HierFn hier, value *lval)
{
  int lvalue,index;
  cell cidx;

  stgget(&index,&cidx); /* mark position in code generator */
  lvalue=(this->*hier)(lval);
  if (lval->ident==iCONSTEXPR)
    stgdel(index,cidx); /* load constant later */
  return lvalue;
}

/*  plnge2
 *
 *  Binary plunge to lower level
 *  Called by: plnge(), plnge_rel(), hier14() and hier1()
 */
void
SC3ExpressionParser::plnge2(void (*oper)(void),
                         HierFn hier,
                         value *lval1,value *lval2)
{
  int index;
  cell cidx;

  stgget(&index,&cidx);             /* mark position in code generator */
  if (lval1->ident==iCONSTEXPR) {   /* constant on left side; it is not yet loaded */
    if (plnge1(hier,lval2))
      rvalue(lval2);                /* load lvalue now */
    else if (lval2->ident==iCONSTEXPR)
      ldconst(lval2->constval<<dbltest(oper,lval2,lval1),sPRI);
    ldconst(lval1->constval<<dbltest(oper,lval2,lval1),sALT);
                   /* ^ doubling of constants operating on integer addresses */
                   /*   is restricted to "add" and "subtract" operators */
  } else {                          /* non-constant on left side */
    pushreg(sPRI);
    if (plnge1(hier,lval2))
      rvalue(lval2);
    if (lval2->ident==iCONSTEXPR) { /* constant on right side */
      if (commutative(oper)) {      /* test for commutative operators */
        value lvaltmp = {0};
        stgdel(index,cidx);         /* scratch pushreg() and constant fetch (then
                                     * fetch the constant again */
        ldconst(lval2->constval<<dbltest(oper,lval1,lval2),sALT);
        /* now, the primary register has the left operand and the secondary
         * register the right operand; swap the "lval" variables so that lval1
         * is associated with the secondary register and lval2 with the
         * primary register, as is the "normal" case.
         */
        lvaltmp=*lval1;
        *lval1=*lval2;
        *lval2=lvaltmp;
      } else {
        ldconst(lval2->constval<<dbltest(oper,lval1,lval2),sPRI);
        popreg(sALT);   /* pop result of left operand into secondary register */
      } /* if */
    } else {            /* non-constants on both sides */
      popreg(sALT);
      if (dbltest(oper,lval1,lval2))
        cell2addr();                    /* double primary register */
      if (dbltest(oper,lval2,lval1))
        cell2addr_alt();                /* double secondary register */
    } /* if */
  } /* if */
  if (oper) {
    /* If used in an expression, a function should return a value.
     * If the function has been defined, we can check this. If the
     * function was not defined, we can set this requirement (so that
     * a future function definition can check this bit.
     */
    checkfunction(lval1);
    checkfunction(lval2);
    if (lval1->ident==iARRAY || lval1->ident==iREFARRAY) {
      const char *ptr=(lval1->sym!=NULL) ? lval1->sym->name() : "-unknown-";
      error(33,ptr);                    /* array must be indexed */
    } else if (lval2->ident==iARRAY || lval2->ident==iREFARRAY) {
      const char *ptr=(lval2->sym!=NULL) ? lval2->sym->name() : "-unknown-";
      error(33,ptr);                    /* array must be indexed */
    } /* if */
    /* ??? ^^^ should do same kind of error checking with functions */

    /* check whether an "operator" function is defined for the tag names
     * (a constant expression cannot be optimized in that case)
     */
    if (check_userop(oper,lval1->tag,lval2->tag,2,NULL,&lval1->tag)) {
      lval1->ident=iEXPRESSION;
      lval1->constval=0;
    } else if (lval1->ident==iCONSTEXPR && lval2->ident==iCONSTEXPR) {
      /* only constant expression if both constant */
      stgdel(index,cidx);       /* scratch generated code and calculate */
      matchtag(lval1->tag,lval2->tag,FALSE);
      lval1->constval=calc(lval1->constval,oper,lval2->constval,&lval1->boolresult);
    } else {
      // For the purposes of tag matching, we consider the order to be irrelevant.
      if (!checkval_string(lval1, lval2))
        matchtag(lval1->tag, lval2->tag, MATCHTAG_COMMUTATIVE|MATCHTAG_DEDUCE);
      (*oper)();                /* do the (signed) operation */
      lval1->ident=iEXPRESSION;
    } /* if */
  } /* if */
}

static cell flooreddiv(cell a,cell b,int return_remainder)
{
  cell q,r;

  if (b==0) {
    error(29);
    return 0;
  } /* if */
  /* first implement truncated division in a portable way */
  #define IABS(a)       ((a)>=0 ? (a) : (-a))
  q=IABS(a)/IABS(b);
  if ((cell)(a ^ b)<0)
    q=-q;               /* swap sign if either "a" or "b" is negative (but not both) */
  r=a-q*b;              /* calculate the matching remainder */
  /* now "fiddle" with the values to get floored division */
  if (r!=0 && (cell)(r ^ b)<0) {
    q--;
    r+=b;
  } /* if */
  return return_remainder ? r : q;
}

static cell calc(cell left,void (*oper)(),cell right,char *boolresult)
{
  if (oper==ob_or)
    return (left | right);
  else if (oper==ob_xor)
    return (left ^ right);
  else if (oper==ob_and)
    return (left & right);
  else if (oper==ob_eq)
    return (left == right);
  else if (oper==ob_ne)
    return (left != right);
  else if (oper==os_le)
    return *boolresult &= (char)(left <= right), right;
  else if (oper==os_ge)
    return *boolresult &= (char)(left >= right), right;
  else if (oper==os_lt)
    return *boolresult &= (char)(left < right), right;
  else if (oper==os_gt)
    return *boolresult &= (char)(left > right), right;
  else if (oper==os_sar)
    return (left >> (int)right);
  else if (oper==ou_sar)
    return ((ucell)left >> (ucell)right);
  else if (oper==ob_sal)
    return ((ucell)left << (int)right);
  else if (oper==ob_add)
    return (left + right);
  else if (oper==ob_sub)
    return (left - right);
  else if (oper==os_mult)
    return (left * right);
  else if (oper==os_div)
    return flooreddiv(left,right,0);
  else if (oper==os_mod)
    return flooreddiv(left,right,1);
  else
    error(29);  /* invalid expression, assumed 0 (this should never occur) */
  return 0;
}

int lvalexpr(svalue *sval)
{
  memset(sval, 0, sizeof(*sval));

  errorset(sEXPRMARK, 0);
  pushheaplist();
  {
    SC3ExpressionParser parser;
    sval->lvalue = parser.evaluate(&sval->val);
  }
  popheaplist(true);
  errorset(sEXPRRELEASE, 0);

  return sval->val.ident;
}

int expression(cell *val,int *tag,symbol **symptr,int chkfuncresult,value *_lval)
{
  value lval={0};
  pushheaplist();

  SC3ExpressionParser parser;
  if (parser.evaluate(&lval))
    rvalue(&lval);
  /* scrap any arrays left on the heap */
  popheaplist(true);

  if (lval.ident==iCONSTEXPR && val!=NULL)    /* constant expression */
    *val=lval.constval;
  if (tag!=NULL)
    *tag=lval.tag;
  if (symptr!=NULL)
    *symptr=lval.sym;
  if (chkfuncresult)
    checkfunction(&lval);
  if (_lval)
    *_lval=lval;
  return lval.ident;
}

/*  hier14
 *
 *  Lowest hierarchy level (except for the , operator).
 *
 *  Global references: sc_intest        (reffered to only)
 */
int
SC3ExpressionParser::hier14(value *lval1)
{
  int lvalue;
  value lval2={0},lval3={0};
  void (*oper)(void);
  int tok,level;
  cell val;
  char *st;
  int leftarray;

  ke::SaveAndSet<int> bitwise_opercount(&bitwise_opercount_, 0);

  /* initialize the index arrays with unlikely constant indices; note that
   * these indices will only be changed when the array is indexed with a
   * constant, and that negative array indices are invalid (so actually, any
   * negative value would do).
   */
  lvalue=plnge1(&SC3ExpressionParser::hier13,lval1);
  if (lval1->ident==iCONSTEXPR) /* load constant here */
    ldconst(lval1->constval,sPRI);
  tok=lex(&val,&st);
  switch (tok) {
    case taOR:
      oper=ob_or;
      break;
    case taXOR:
      oper=ob_xor;
      break;
    case taAND:
      oper=ob_and;
      break;
    case taADD:
      oper=ob_add;
      break;
    case taSUB:
      oper=ob_sub;
      break;
    case taMULT:
      oper=os_mult;
      break;
    case taDIV:
      oper=os_div;
      break;
    case taMOD:
      oper=os_mod;
      break;
    case taSHRU:
      oper=ou_sar;
      break;
    case taSHR:
      oper=os_sar;
      break;
    case taSHL:
      oper=ob_sal;
      break;
    case '=':           /* simple assignment */
      oper=NULL;
      if (sc_intest)
        error(211);     /* possibly unintended assignment */
      break;
    default:
      lexpush();
      return lvalue;
  } /* switch */

  /* if we get here, it was an assignment; first check a few special cases
   * and then the general */
  if (lval1->ident==iARRAYCHAR) {
    /* special case, assignment to packed character in a cell is permitted */
    lvalue=TRUE;
  } else if (lval1->ident==iARRAY || lval1->ident==iREFARRAY) {
    /* array assignment is permitted too (with restrictions) */
    if (oper)
      return error(23); /* array assignment must be simple assigment */
    assert(lval1->sym!=NULL);
    if (array_totalsize(lval1->sym)==0)
      return error(46,lval1->sym->name());        /* unknown array size */
    lvalue=TRUE;
  } /* if */

  /* operand on left side of assignment must be lvalue */
  if (!lvalue)
    return error(22);                   /* must be lvalue */
  /* may not change "constant" parameters */
  assert(lval1->sym || lval1->accessor);
  if (lval1->sym && (lval1->sym->usage & uCONST) != 0)
    return error(22);           /* assignment to const argument */

  lval3=*lval1;         /* save symbol to enable storage of expresion result */
  if (lval1->ident==iARRAYCELL || lval1->ident==iARRAYCHAR
      || lval1->ident==iARRAY || lval1->ident==iREFARRAY)
  {
    /* if indirect fetch: save PRI (cell address) */
    if (oper) {
      pushreg(sPRI);
      rvalue(lval1);
    } /* if */
    plnge2(oper,&SC3ExpressionParser::hier14,lval1,&lval2);
    if (oper)
      popreg(sALT);
  } else if (lval1->ident == iACCESSOR) {
    pushreg(sPRI);
    if (oper) {
      rvalue(lval1);
      plnge2(oper,&SC3ExpressionParser::hier14,lval1,&lval2);
    } else {
      if (hier14(&lval2))
        rvalue(&lval2);         /* instead of plnge2(). */
      else if (lval2.ident==iVARIABLE)
        lval2.ident=iEXPRESSION;/* mark as "rvalue" if it is not an "lvalue" */
      checkfunction(&lval2);
    }
    popreg(sALT);
  } else {
    if (oper){
      rvalue(lval1);
      plnge2(oper,&SC3ExpressionParser::hier14,lval1,&lval2);
    } else {
      /* if direct fetch and simple assignment: no "push"
       * and "pop" needed -> call hier14() directly, */
      if (hier14(&lval2))
        rvalue(&lval2);         /* instead of plnge2(). */
      else if (lval2.ident==iVARIABLE)
        lval2.ident=iEXPRESSION;/* mark as "rvalue" if it is not an "lvalue" */
      checkfunction(&lval2);
      /* check whether lval2 and lval3 (old lval1) refer to the same variable */
      if (lval2.ident==iVARIABLE && lval3.ident==lval2.ident && lval3.sym==lval2.sym) {
        assert(lval3.sym!=NULL);
        error(226,lval3.sym->name());     /* self-assignment */
      } /* if */
    } /* if */
  } /* if */
  /* Array elements are sometimes considered as sub-arrays --when the
   * array index is an enumeration field and the enumeration size is greater
   * than 1. If the expression on the right side of the assignment is a cell,
   * or if an operation is in effect, this does not apply.
   */
  leftarray= lval3.ident==iARRAY || lval3.ident==iREFARRAY
             || ((lval3.ident==iARRAYCELL || lval3.ident==iARRAYCHAR)
                 && lval3.constval>1 && lval3.sym->dim.array.level==0
                 && !oper && (lval2.ident==iARRAY || lval2.ident==iREFARRAY));
  if (leftarray) {
    /* Left operand is an array, right operand should be an array variable
     * of the same size and the same dimension, an array literal (of the
     * same size) or a literal string. For single-dimensional arrays without
     * tag for the index, it is permitted to assign a smaller array into a
     * larger one (without warning). This is to make it easier to work with
     * strings.
     */
    int exactmatch=TRUE;
    int idxtag=0;
    int ltlength=(int)lval3.sym->dim.array.length;
    if ((lval3.ident==iARRAYCELL || lval3.ident==iARRAYCHAR)
        && lval3.constval>0 && lval3.sym->dim.array.level==0)
    {
      ltlength=(int)lval3.constval;
    } /* if */
    if (lval2.ident!=iARRAY && lval2.ident!=iREFARRAY
        && (lval2.sym==NULL || lval2.constval<=0))
      error(33,lval3.sym->name());        /* array must be indexed */
    if (lval2.sym!=NULL) {
      if (lval2.constval==0) {
        val=lval2.sym->dim.array.length;/* array variable */
      } else {
        val=lval2.constval;
        if (lval2.sym->dim.array.level!=0)
          error(28,lval2.sym->name());
      } /* if */
      level=lval2.sym->dim.array.level;
      idxtag=lval2.sym->x.tags.index;
      if (level==0 && idxtag==0 && lval3.sym->x.tags.index==0)
        exactmatch=FALSE;
    } else {
      val=lval2.constval;               /* literal array */
      level=0;
      /* If val is negative, it means that lval2 is a literal string.
       * The string array size may be smaller than the destination
       * array, provided that the destination array does not have an
       * index tag.
       */
      if (val<0) {
        val=-val;
        if (lval3.sym->x.tags.index==0)
          exactmatch=FALSE;
      } /* if */
    } /* if */
    if (lval3.sym->dim.array.level!=level)
      return error(47); /* array dimensions must match */
    else if (ltlength<val || (exactmatch && ltlength>val) || val==0)
      return error(47); /* array sizes must match */
    else if (lval3.ident!=iARRAYCELL && !matchtag(lval3.sym->x.tags.index,idxtag,MATCHTAG_COERCE|MATCHTAG_SILENT))
      error(229,(lval2.sym!=NULL) ? lval2.sym->name() : lval3.sym->name()); /* index tag mismatch */
    if (level>0) {
      /* check the sizes of all sublevels too */
      symbol *sym1 = lval3.sym;
      symbol *sym2 = lval2.sym;
      int i;
      error(23);
      assert(sym1!=NULL && sym2!=NULL);
      /* ^^^ sym2 must be valid, because only variables can be
       *     multi-dimensional (there are no multi-dimensional literals),
       *     sym1 must be valid because it must be an lvalue
       */
      assert(exactmatch);
      for (i=0; i<level; i++) {
        sym1=sym1->array_child();
        sym2=sym2->array_child();
        assert(sym1!=NULL && sym2!=NULL);
        /* ^^^ both arrays have the same dimensions (this was checked
         *     earlier) so the dependend should always be found
         */
        if (sym1->dim.array.length!=sym2->dim.array.length)
          error(47);    /* array sizes must match */
        else if (!matchtag(sym1->x.tags.index,sym2->x.tags.index,MATCHTAG_COERCE|MATCHTAG_SILENT))
          error(229,sym2->name());  /* index tag mismatch */
      } /* for */
      /* get the total size in cells of the multi-dimensional array */
      val=array_totalsize(lval3.sym);
      assert(val>0);    /* already checked */
    } /* if */
  } else {
    /* left operand is not an array, right operand should then not be either */
    if (lval2.ident==iARRAY || lval2.ident==iREFARRAY)
      error(6);         /* must be assigned to an array */
  } /* if */
  if (leftarray) {
    memcopy(val*sizeof(cell));
  } else {
    check_userop(NULL,lval2.tag,lval3.tag,2,&lval3,&lval2.tag);
    store(&lval3);      /* now, store the expression result */
  } /* if */
  if (!oper && !checkval_string(&lval3, &lval2)) {
    if ((lval3.tag == pc_tag_string && lval2.tag != pc_tag_string) ||
        (lval3.tag != pc_tag_string && lval2.tag == pc_tag_string))
    {
      error(179, type_to_name(lval3.tag), type_to_name(lval2.tag));
    } else {
      matchtag(lval3.tag,lval2.tag,TRUE);
    }
  }
  if (lval3.sym)
    markusage(lval3.sym,uWRITTEN);
  sideeffect=TRUE;
  lval1->ident=iEXPRESSION;
  return FALSE;         /* expression result is never an lvalue */
}

int
SC3ExpressionParser::hier13(value *lval)
{
  int lvalue=plnge1(&SC3ExpressionParser::hier12,lval);
  if (matchtoken('?')) {
    int flab1=getlabel();
    int flab2=getlabel();
    value lval2={0};
    int array1,array2;
    cell_t total1 = 0;
    cell_t total2 = 0;
    
    pushheaplist();
    if (lvalue) {
      rvalue(lval);
    } else if (lval->ident==iCONSTEXPR) {
      ldconst(lval->constval,sPRI);
      error(lval->constval ? 206 : 205);        /* redundant test */
    } /* if */
    jmp_eq0(flab1);             /* go to second expression if primary register==0 */
    PUSHSTK_I(sc_allowtags);
    sc_allowtags=FALSE;         /* do not allow tagnames here (colon is a special token) */
    if (hier13(lval))
      rvalue(lval);
    if (lval->ident==iCONSTEXPR)        /* load constant here */
      ldconst(lval->constval,sPRI);
    sc_allowtags=(short)POPSTK_I();     /* restore */
    if ((total1 = pop_static_heaplist())) {
      setheap_save(total1 * sizeof(cell));
    }
    pushheaplist();
    jumplabel(flab2);
    setlabel(flab1);
    needtoken(':');
    if (hier13(&lval2))
      rvalue(&lval2);
    if (lval2.ident==iCONSTEXPR)        /* load constant here */
      ldconst(lval2.constval,sPRI);
    array1= (lval->ident==iARRAY || lval->ident==iREFARRAY);
    array2= (lval2.ident==iARRAY || lval2.ident==iREFARRAY);
    if (!array1 && array2) {
      const char *ptr = "-unknown-";
      if (lval->sym != NULL)
        ptr = lval->sym->name();
      error(33,ptr);            /* array must be indexed */
    } else if (array1 && !array2) {
      const char *ptr = "-unknown-";
      if (lval2.sym != NULL)
        ptr = lval2.sym->name();
      error(33,ptr);            /* array must be indexed */
    } /* if */
    /* ??? if both are arrays, should check dimensions */
    matchtag(lval->tag,lval2.tag,FALSE);
    if ((total2 = pop_static_heaplist())) {
      setheap_save(total2 * sizeof(cell));
    }
    setlabel(flab2);
    if ((array1 && array2) && (total1 && total2)) {
      markheap(MEMUSE_DYNAMIC, 0);
    }
    /* If both sides are arrays, we should return the maximal as the lvalue.
     * Otherwise we could buffer overflow and the compiler is too stupid.
     * Literal strings have a constval == -(num_cells) so the cmp is flipped.
     */
    if (lval->ident==iARRAY && lval2.ident==iARRAY
        && lval->constval < 0
        && lval->constval > lval2.constval) {
      *lval = lval2;
    }
    if (lval->ident==iARRAY)
      lval->ident=iREFARRAY;    /* iARRAY becomes iREFARRAY */
    else if (lval->ident!=iREFARRAY)
      lval->ident=iEXPRESSION;  /* iREFARRAY stays iREFARRAY, rest becomes iEXPRESSION */
    return FALSE;               /* conditional expression is no lvalue */
  } else {
    return lvalue;
  } /* if */
}

int
SC3ExpressionParser::hier12(value *lval)
{
  return skim(list12,jmp_ne0,1,0,&SC3ExpressionParser::hier11,lval);
}

int
SC3ExpressionParser::hier11(value *lval)
{
  return skim(list11,jmp_eq0,0,1,&SC3ExpressionParser::hier10,lval);
}

int
SC3ExpressionParser::hier10(value *lval)
{ /* ==, != */
  return plnge(list10,15,&SC3ExpressionParser::hier9,lval,"bool",TRUE);
}                  /* ^ this variable is the starting index in the op1[]
                    *   array of the operators of this hierarchy level */
int
SC3ExpressionParser::hier9(value *lval)
{ /* <=, >=, <, > */
  return plnge_rel(list9,11,&SC3ExpressionParser::hier8,lval);
}

int
SC3ExpressionParser::hier8(value *lval)
{ /* | */
  return plnge(list8,10,&SC3ExpressionParser::hier7,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier7(value *lval)
{ /* ^ */
  return plnge(list7,9,&SC3ExpressionParser::hier6,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier6(value *lval)
{ /* & */
  return plnge(list6,8,&SC3ExpressionParser::hier5,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier5(value *lval)
{ /* <<, >>, >>> */
  return plnge(list5,5,&SC3ExpressionParser::hier4,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier4(value *lval)
{ /* +, - */
  return plnge(list4,3,&SC3ExpressionParser::hier3,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier3(value *lval)
{ /* *, /, % */
  return plnge(list3,0,&SC3ExpressionParser::hier2,lval,NULL,FALSE);
}

int
SC3ExpressionParser::hier2(value *lval)
{
  int lvalue,tok;
  int tag;
  cell val;
  char *st;
  int saveresult;

  tok=lex(&val,&st);
  switch (tok) {
  case tINC:                    /* ++lval */
    if (!hier2(lval))
      return error(22);         /* must be lvalue */
    if (lval->ident != iACCESSOR) {
      assert(lval->sym!=NULL);
      if ((lval->sym->usage & uCONST)!=0)
        return error(22);         /* assignment to const argument */
      if (!check_userop(user_inc,lval->tag,0,1,lval,&lval->tag))
        inc(lval);                /* increase variable first */
      rvalue(lval);               /* and read the result into PRI */
    } else {
      pushreg(sPRI);
      invoke_getter(lval->accessor);
      if (!check_userop(user_inc,lval->tag,0,1,lval,&lval->tag))
        inc_pri();
      popreg(sALT);
      invoke_setter(lval->accessor, TRUE);
    }
    lval->ident = iEXPRESSION;
    sideeffect=TRUE;
    return FALSE;               /* result is no longer lvalue */
  case tDEC:                    /* --lval */
    if (!hier2(lval))
      return error(22);         /* must be lvalue */
    if (lval->ident != iACCESSOR) {
      assert(lval->sym!=NULL);
      if ((lval->sym->usage & uCONST)!=0)
        return error(22);         /* assignment to const argument */
      if (!check_userop(user_dec,lval->tag,0,1,lval,&lval->tag))
        dec(lval);                /* decrease variable first */
      rvalue(lval);               /* and read the result into PRI */
    } else {
      pushreg(sPRI);
      invoke_getter(lval->accessor);
      if (!check_userop(user_dec,lval->tag,0,1,lval,&lval->tag))
        dec_pri();
      popreg(sALT);
      invoke_setter(lval->accessor, TRUE);
    }
    lval->ident = iEXPRESSION;
    sideeffect=TRUE;
    return FALSE;               /* result is no longer lvalue */
  case '~':                     /* ~ (one's complement) */
    if (hier2(lval))
      rvalue(lval);
    invert();                   /* bitwise NOT */
    lval->constval=~lval->constval;
    if (lval->ident != iCONSTEXPR)
      lval->ident = iEXPRESSION;
    return FALSE;
  case '!':                     /* ! (logical negate) */
    if (hier2(lval))
      rvalue(lval);
    if (check_userop(lneg,lval->tag,0,1,NULL,&lval->tag)) {
      lval->ident=iEXPRESSION;
      lval->constval=0;
    } else {
      lneg();                   /* 0 -> 1,  !0 -> 0 */
      lval->constval=!lval->constval;
      if (lval->ident != iCONSTEXPR)
        lval->ident = iEXPRESSION;
      lval->tag=pc_addtag("bool");
    } /* if */
    return FALSE;
  case '-':                     /* unary - (two's complement) */
    if (hier2(lval))
      rvalue(lval);
    /* make a special check for a constant expression with the tag of a
     * rational number, so that we can simple swap the sign of that constant.
     */
    if (lval->ident==iCONSTEXPR && lval->tag==sc_rationaltag && sc_rationaltag!=0) {
      if (rational_digits==0) {
#if PAWN_CELL_SIZE==32
          float *f = (float *)&lval->constval;
#elif PAWN_CELL_SIZE==64
          double *f = (double *)&lval->constval;
#else
# error Unsupported cell size
#endif
        *f= - *f; /* this modifies lval->constval */
      } else {
        /* the negation of a fixed point number is just an integer negation */
        lval->constval=-lval->constval;
      } /* if */
    } else if (check_userop(neg,lval->tag,0,1,NULL,&lval->tag)) {
      lval->ident=iEXPRESSION;
      lval->constval=0;
    } else {
      neg();                    /* arithmic negation */
      lval->constval=-lval->constval;
      if (lval->ident != iCONSTEXPR)
        lval->ident = iEXPRESSION;
    } /* if */
    return FALSE;
  case tNEW:                    /* call nullable methodmap constructor */
  {
    tok = lex(&val, &st);
    if (tok != tSYMBOL)
      return error(20, st);     /* illegal symbol name */

    symbol *target = NULL;
    methodmap_t *methodmap = methodmap_find_by_name(st);
    if (!methodmap)
      error(116, st);
    else if (!methodmap->must_construct_with_new())
      error(171, methodmap->name);
    else if (!methodmap->ctor)
      error(172, methodmap->name);
    else
      target = methodmap->ctor->target;

    if (!target) {
      needtoken('(');
      int depth = 1;
      // Eat tokens until we get a newline or EOF or ')' or ';'
      while (true) {
        if (peek_same_line() == tEOL)
          return FALSE;
        if ((tok = lex(&val, &st)) == 0)
          return FALSE;
        if (tok == ')') {
          if (--depth == 0)
            return FALSE;
        }
        if (tok == ';')
          return FALSE;
        if (tok == '(')
          depth++;
      }
    }

    needtoken('(');
    callfunction(target, NULL, lval, TRUE);
    return FALSE;
  }
  case tLABEL:                  /* tagname override */
  {
    tag=pc_addtag(st);
    if (sc_require_newdecls) {
      // Warn: old style cast used when newdecls pragma is enabled
      error(240, st, type_to_name(tag));
    }
    lvalue=hier2(lval);
    Type* ltype = gTypes.find(lval->tag);
    Type* atype = gTypes.find(tag);
    if (ltype->isObject() || atype->isObject()) {
      matchtag(tag, lval->tag, MATCHTAG_COERCE);
    } else if (ltype->isFunction() != atype->isFunction()) {
      // Warn: unsupported cast.
      error(237);
    }
    lval->tag=tag;
    return lvalue;
  }
  case tDEFINED:
    val = parse_defined();
    clear_value(lval);
    lval->ident=iCONSTEXPR;
    lval->constval= val;
    lval->tag=pc_addtag("bool");
    ldconst(lval->constval,sPRI);
    return FALSE;
  case tSIZEOF:
    val = parse_sizeof();
    clear_value(lval);
    lval->ident=iCONSTEXPR;
    lval->constval=val;
    ldconst(lval->constval,sPRI);
    return FALSE;
  case tCELLSOF:
    val = parse_cellsof();
    clear_value(lval);
    lval->ident=iCONSTEXPR;
    lval->constval=val;
    ldconst(lval->constval,sPRI);
    return FALSE;
  case tTAGOF:
    val = parse_tagof();
    clear_value(lval);
    lval->ident=iCONSTEXPR;
    lval->constval=val;
    ldconst(lval->constval,sPRI);
    return FALSE;
  default:
    lexpush();
    lvalue=hier1(lval);
    /* check for postfix operators */
    if (matchtoken(';')) {
      /* Found a ';', do not look further for postfix operators */
      lexpush();                /* push ';' back after successful match */
      return lvalue;
    } else if (matchtoken(tTERM)) {
      /* Found a newline that ends a statement (this is the case when
       * semicolons are optional). Note that an explicit semicolon was
       * handled above. This case is similar, except that the token must
       * not be pushed back.
       */
      return lvalue;
    } else {
      tok=lex(&val,&st);
      switch (tok) {
      case tINC:                /* lval++ */
        if (!lvalue)
          return error(22);     /* must be lvalue */
        if (lval->ident != iACCESSOR) {
          assert(lval->sym!=NULL);
          if ((lval->sym->usage & uCONST)!=0)
            return error(22);     /* assignment to const argument */
          /* on incrementing array cells, the address in PRI must be saved for
           * incremening the value, whereas the current value must be in PRI
           * on exit.
           */
          saveresult= (lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR);
          if (saveresult)
            pushreg(sPRI);        /* save address in PRI */
          rvalue(lval);           /* read current value into PRI */
          if (saveresult)
            swap1();              /* save PRI on the stack, restore address in PRI */
          if (!check_userop(user_inc,lval->tag,0,1,lval,&lval->tag))
            inc(lval);            /* increase variable afterwards */
          if (saveresult)
            popreg(sPRI);         /* restore PRI (result of rvalue()) */
        } else {
          pushreg(sPRI);    // save obj
          invoke_getter(lval->accessor);
          move_alt();     // alt = oldval
          swap1();        // pri = saved obj, stack = [oldval]
          pushreg(sPRI);  // pri = obj, alt = oldval, stack = [obj, oldval]
          moveto1();      // pri = oldval, stack = [obj, oldval]

          // check_userop on an iACCESSOR acts as though the value is an rvalue.
          if (!check_userop(user_inc, lval->tag, 0, 1, lval, &lval->tag))
            inc_pri();

          popreg(sALT);
          invoke_setter(lval->accessor, FALSE);
          popreg(sPRI);
        }
        lval->ident = iEXPRESSION;
        sideeffect=TRUE;
        return FALSE;           /* result is no longer lvalue */
      case tDEC:                /* lval-- */
        if (!lvalue)
          return error(22);     /* must be lvalue */
        if (lval->ident != iACCESSOR) {
          assert(lval->sym!=NULL);
          if ((lval->sym->usage & uCONST)!=0)
            return error(22);     /* assignment to const argument */
          saveresult= (lval->ident==iARRAYCELL || lval->ident==iARRAYCHAR);
          if (saveresult)
            pushreg(sPRI);        /* save address in PRI */
          rvalue(lval);           /* read current value into PRI */
          if (saveresult)
            swap1();              /* save PRI on the stack, restore address in PRI */
          if (!check_userop(user_dec,lval->tag,0,1,lval,&lval->tag))
            dec(lval);            /* decrease variable afterwards */
          if (saveresult)
            popreg(sPRI);         /* restore PRI (result of rvalue()) */
        } else {
          pushreg(sPRI);    // save obj
          invoke_getter(lval->accessor);
          move_alt();     // alt = oldval
          swap1();        // pri = saved obj, stack = [oldval]
          pushreg(sPRI);  // pri = obj, alt = oldval, stack = [obj, oldval]
          moveto1();      // pri = oldval, stack = [obj, oldval]

          // check_userop on an iACCESSOR acts as though the value is an rvalue.
          if (!check_userop(user_dec, lval->tag, 0, 1, lval, &lval->tag))
            dec_pri();

          popreg(sALT);
          invoke_setter(lval->accessor, FALSE);
          popreg(sPRI);
        }
        lval->ident = iEXPRESSION;
        sideeffect=TRUE;
        return FALSE;
      default:
        lexpush();
        return lvalue;
      } /* switch */
    } /* if */
  } /* switch */
}

static symbol *
fake_function_for_method(methodmap_t *map, const char *lexstr)
{
  // Fetch a fake function so errors aren't as crazy.
  char tmpname[METHOD_NAMEMAX + 1];
  strcpy(tmpname, map->name);
  strcat(tmpname, ".");
  strcat(tmpname, lexstr);
  tmpname[sNAMEMAX] = '\0';
  return fetchfunc(tmpname);
}

enum FieldExprResult
{
  FER_Fail,
  FER_Accessor,
  FER_CallFunction,
  FER_CallMethod
};

static FieldExprResult
field_expression(svalue &thisval, value *lval, symbol **target, methodmap_method_t **methodp)
{
  // Catch invalid calls early so we don't compile with a tag mismatch.
  switch (thisval.val.ident) {
    case iARRAY:
    case iREFARRAY:
      error(106);
      break;

    case iFUNCTN:
      error(107);
      break;
  }

  cell lexval;
  char *lexstr;
  if (!needtoken(tSYMBOL))
    return FER_Fail;
  tokeninfo(&lexval, &lexstr);

  if (thisval.val.ident == iMETHODMAP) {
    methodmap_t *map = thisval.val.sym->methodmap;
    methodmap_method_t *method = methodmap_find_method(map, lexstr);
    if (!method) {
      error(105, map->name, lexstr);
      *target = fake_function_for_method(map, lexstr);
      return FER_CallFunction;
    }

    if (!method->is_static)
      error(176, method->name, map->name);
    *target = method->target;
    return FER_CallFunction;
  }

  methodmap_t *map = gTypes.find(thisval.val.tag)->asMethodmap();
  if (!map) {
    error(104, pc_tagname(thisval.val.tag));
    return FER_Fail;
  }

  methodmap_method_t *method;
  if ((method = methodmap_find_method(map, lexstr)) == NULL) {
    error(105, map->name, lexstr);
    *target = fake_function_for_method(map, lexstr);
    return FER_CallFunction;
  }

  if (method && (method->getter || method->setter)) {
    if (thisval.lvalue)
      rvalue(lval);
    clear_value(lval);
    lval->ident = iACCESSOR;
    lval->tag = method->property_tag();
    lval->accessor = method;
    *methodp = method;
    return FER_Accessor;
  }

  *target = method->target;

  if (method->is_static) {
    error(177, method->name, map->name, method->name);
    return FER_CallFunction;
  }

  *methodp = method;
  return FER_CallMethod;
}

int
SC3ExpressionParser::parse_view_as(value* lval)
{
  needtoken('<');
  int tag = 0;
  {
    token_t tok;
    lextok(&tok);
    if (!parse_new_typename(&tok, &tag))
      tag = 0;
  }
  needtoken('>');

  if (tag == pc_tag_void)
    error(144);

  int paren = needtoken('(');

  int lvalue = hier14(lval);
  if (paren)
    needtoken(')');
  else
    matchtoken(')');

  Type* ltype = gTypes.find(lval->tag);
  Type* atype = gTypes.find(tag);
  if (ltype->isObject() || atype->isObject()) {
    matchtag(tag, lval->tag, MATCHTAG_COERCE);
  } else if (ltype->isFunction() != atype->isFunction()) {
    // Warn: unsupported cast.
    error(237);
  } else if (lval->sym && lval->sym->tag == pc_tag_void) {
    error(89);
  }
  lval->tag = tag;
  return lvalue;
}

/*  hier1
 *
 *  The highest hierarchy level: it looks for pointer and array indices
 *  and function calls.
 *  Generates code to fetch a pointer value if it is indexed and code to
 *  add to the pointer value or the array address (the address is already
 *  read at primary()). It also generates code to fetch a function address
 *  if that hasn't already been done at primary() (check lval[4]) and calls
 *  callfunction() to call the function.
 */
int
SC3ExpressionParser::hier1(value *lval1)
{
  int lvalue,index,tok;
  cell val,cidx;
  value lval2={0};
  char *st;
  char close;
  symbol *sym;
  int magic_string=0;
  symbol dummysymbol;   /* for changing the index tags in case of enumerated pseudo-arrays */

  int symtok = 0;
  symbol* cursym = nullptr;
  if (matchtoken(tVIEW_AS)) {
    lvalue = parse_view_as(lval1);
  } else {
    lvalue=primary(lval1);
    symtok=tokeninfo(&val,&st);   /* get token read by primary() */
    cursym=lval1->sym;
  }

restart:
  sym=cursym;

  if (lval1->ident == iMETHODMAP &&
      !(lexpeek('.') || lexpeek('(')))
  {
    // Cannot use methodmap as an rvalue/lvalue.
    error(174, sym ? sym->name() : "(unknown)");

    lval1->ident = iCONSTEXPR;
    lval1->tag = 0;
    lval1->constval = 0;
  }

  if (matchtoken('[') || matchtoken('(') || matchtoken('.')) {
    tok=tokeninfo(&val,&st);    /* get token read by matchtoken() */
    if (lvalue && lval1->ident == iACCESSOR) {
      rvalue(lval1);
      lvalue = FALSE;
    }
    magic_string = (sym && (sym->tag == pc_tag_string && sym->dim.array.level == 0));
    if (tok=='[') { /* subscript */
      if (sym==NULL && symtok!=tSYMBOL) {
        /* we do not have a valid symbol and we appear not to have read a valid
         * symbol name (so it is unlikely that we would have read a name of an
         * undefined symbol) */
        error(29);                /* expression error, assumed 0 */
        lexpush();                /* analyse '(' or '[' again later */
        return FALSE;
      } /* if */
      close = ']';
      if (sym==NULL) {  /* sym==NULL if lval is a constant or a literal */
        error(28,"<no variable>");  /* cannot subscript */
        needtoken(close);
        return FALSE;
      } else if (sym->ident!=iARRAY && sym->ident!=iREFARRAY){
        error(28,sym->name());    /* cannot subscript, variable is not an array */
        needtoken(close);
        return FALSE;
      } /* if */
      /* set the tag to match (enumeration fields as indices) */
      stgget(&index,&cidx);     /* mark position in code generator */
      pushreg(sPRI);            /* save base address of the array */
      if (hier14(&lval2))       /* create expression for the array index */
        rvalue(&lval2);
      if (lval2.ident==iARRAY || lval2.ident==iREFARRAY)
        error(33,lval2.sym->name());      /* array must be indexed */
      needtoken(close);
      if ((sym->usage & uENUMROOT))
        matchtag(sym->x.tags.index,lval2.tag,TRUE);
      if (lval2.ident==iCONSTEXPR) {    /* constant expression */
        stgdel(index,cidx);             /* scratch generated code */
        if (!(sym->tag == pc_tag_string && sym->dim.array.level == 0)) {
          /* normal array index */
          if (lval2.constval<0 || (sym->dim.array.length!=0 && sym->dim.array.length<=lval2.constval))
            error(32,sym->name());        /* array index out of bounds */
          if (lval2.constval!=0) {
            /* don't add offsets for zero subscripts */
            #if PAWN_CELL_SIZE==16
              ldconst(lval2.constval<<1,sALT);
            #elif PAWN_CELL_SIZE==32
              ldconst(lval2.constval<<2,sALT);
            #elif PAWN_CELL_SIZE==64
              ldconst(lval2.constval<<3,sALT);
            #else
              #error Unsupported cell size
            #endif
            ob_add();
          } /* if */
        } else {
          /* character index */
          if (lval2.constval<0 || (sym->dim.array.length!=0
              && sym->dim.array.length*((8*sizeof(cell))/sCHARBITS)<=(ucell)lval2.constval))
            error(32,sym->name());        /* array index out of bounds */
          if (lval2.constval!=0) {
            /* don't add offsets for zero subscripts */
            #if sCHARBITS==16
              ldconst(lval2.constval<<1,sALT);/* 16-bit character */
            #else
              ldconst(lval2.constval,sALT);   /* 8-bit character */
            #endif
            ob_add();
          } /* if */
        } /* if */
        /* if the array index is a field from an enumeration, get the tag name
         * from the field and save the size of the field too.
         */
        assert(lval2.sym==NULL || lval2.sym->dim.array.level==0);
        if (lval2.sym &&
            lval2.sym->parent() &&
            lval2.sym->dim.array.length > 0 &&
            sym->dim.array.level==0)
        {
          lval1->tag=lval2.sym->x.tags.index;
          lval1->constval=lval2.sym->dim.array.length;
        } /* if */
      } else {
        /* array index is not constant */
        if (!magic_string) {
          if (sym->dim.array.length!=0)
            ffbounds(sym->dim.array.length-1);  /* run time check for array bounds */
          else
            ffbounds();
          cell2addr();  /* normal array index */
        } else {
          if (sym->dim.array.length!=0)
            ffbounds(sym->dim.array.length*(32/sCHARBITS)-1);
          else
            ffbounds();
          char2addr();  /* character array index */
        } /* if */
        popreg(sALT);
        ob_add();       /* base address was popped into secondary register */
      } /* if */
      /* the indexed item may be another array (multi-dimensional arrays) */
      assert(cursym==sym && sym!=NULL); /* should still be set */
      if (sym->dim.array.level>0) {
        assert(cursym==lval1->sym);
        /* read the offset to the subarray and add it to the current address */
        lval1->ident=iARRAYCELL;
        pushreg(sPRI);          /* the optimizer makes this to a MOVE.alt */
        rvalue(lval1);
        popreg(sALT);
        ob_add();
        /* adjust the "value" structure and find the referenced array */
        lval1->ident=iREFARRAY;
        lval1->sym=sym->array_child();
        assert(lval1->sym!=NULL);
        assert(lval1->sym->dim.array.level==sym->dim.array.level-1);
        cursym=lval1->sym;
        /* try to parse subsequent array indices */
        lvalue=FALSE;   /* for now, a iREFARRAY is no lvalue */
        goto restart;
      } /* if */
      assert(sym->dim.array.level==0);
      /* set type to fetch... INDIRECTLY */
      if (sym->tag == pc_tag_string) {
        lval1->ident = iARRAYCHAR;
      } else {
        lval1->ident= iARRAYCELL;
      }
      /* if the array index is a field from an enumeration, get the tag name
       * from the field and save the size of the field too. Otherwise, the
       * tag is the one from the array symbol.
       */
      if (lval2.ident==iCONSTEXPR &&
          lval2.sym &&
          lval2.sym->parent() &&
          lval2.sym->dim.array.length > 0 &&
          sym->dim.array.level == 0)
      {
        lval1->tag=lval2.sym->x.tags.index;
        lval1->constval=lval2.sym->dim.array.length;
        if (lval2.tag==sym->x.tags.index && lval1->constval>1 && matchtoken('[')) {
          /* an array indexed with an enumeration field may be considered a sub-array */
          lexpush();
          lvalue=FALSE;   /* for now, a iREFARRAY is no lvalue */
          lval1->ident=iREFARRAY;
          /* initialize a dummy symbol, which is a copy of the current symbol,
           * but with an adjusted index tag
           */
          assert(sym!=NULL);
          new (&dummysymbol) symbol(*sym);
          /* get the tag of the root of the enumeration */
          assert(lval2.sym!=NULL);
          dummysymbol.x.tags.index=lval2.sym->x.tags.field;
          dummysymbol.dim.array.length=lval2.sym->dim.array.length;
          cursym=&dummysymbol;
          /* recurse */
          goto restart;
        } /* if */
      } else {
        assert(sym!=NULL);
        if (cursym!=&dummysymbol)
          lval1->tag=sym->tag;
        lval1->constval=0;
      } /* if */

      /* a cell in an array is an lvalue, a character in an array is not
       * always a *valid* lvalue */
      lvalue = TRUE;

      // If there's a call/fetch coming up, keep parsing.
      if (matchtoken('.')) {
        lexpush();
        goto restart;
      }

      return lvalue;
    } else {            /* tok=='(' -> function(...) */
      svalue thisval;
      thisval.val = *lval1;
      thisval.lvalue = lvalue;

      svalue *implicitthis = nullptr;
      methodmap_method_t* method = nullptr;
      if (tok == '.') {
        switch (field_expression(thisval, lval1, &sym, &method)) {
          case FER_Fail:
          case FER_CallFunction:
            break;
          case FER_CallMethod:
            implicitthis = &thisval;
            break;
          case FER_Accessor:
            lvalue = TRUE;
            goto restart;
          default:
            assert(false);
        }

        // If we don't find a '(' next, just fail to compile for now -- and
        // don't even try to do a function call, just restart the parse loop.
        if (!needtoken('('))
          goto restart;

        tok = '(';
      }

      if (method && method->parent->ctor == method) {
        implicitthis = nullptr;
        error(84, method->parent->name);
      }

      assert(tok=='(');
      if (sym==NULL || (sym->ident!=iFUNCTN)) {
        if (sym && sym->ident == iMETHODMAP && sym->methodmap) {
          if (!sym->methodmap->ctor) {
            // Immediately fatal - no function to call.
            return error(172, sym->name());
          }
          if (sym->methodmap->must_construct_with_new()) {
            // Keep going, this is basically a style thing.
            error(170, sym->methodmap->name);
          }

          sym = sym->methodmap->ctor->target;
        } else if (sym==NULL && sc_status==statFIRST) {
          /* could be a "use before declaration"; in that case, create a stub
           * function so that the usage can be marked.
           */
          sym=fetchfunc(lastsymbol);
          if (sym==NULL)
            error(FATAL_ERROR_OOM);
          markusage(sym,uREAD);
        } else {
          return error(12);           /* invalid function call */
        }
      } else if ((sym->usage & uMISSING)!=0) {
        char symname[2*sNAMEMAX+16];  /* allow space for user defined operators */
        funcdisplayname(symname,sym->name());
        error(4,symname);             /* function not defined */
      } /* if */

      callfunction(sym,implicitthis,lval1,TRUE);
      if (lexpeek('.')) {
        lvalue = FALSE;
        goto restart;
      }
      return FALSE;             /* result of function call is no lvalue */
    } /* if */
  } /* if */
  if (sym!=NULL && lval1->ident==iFUNCTN) {
    assert(sym->ident==iFUNCTN);

    if (sym->usage & uNATIVE) {
      error(76);
      return FALSE;
    }
    if (sym->array_return()) {
      error(182);
      return FALSE;
    }

    funcenum_t *fe = funcenum_for_symbol(sym);
    markusage(sym, uCALLBACK);
    if (sc_status!=statSKIP)
      markusage(sym, uREAD);

    // Get address into pri.
    load_glbfn(sym);

    // New-style "closure".
    lval1->sym = NULL;
    lval1->ident = iEXPRESSION;
    lval1->constval = 0;
    lval1->tag = fe->tag;
    return FALSE;
  } /* if */
  return lvalue;
}

/*  primary
 *
 *  Returns 1 if the operand is an lvalue (everything except arrays, functions
 *  constants and -of course- errors).
 *  Generates code to fetch the address of arrays. Code for constants is
 *  already generated by constant().
 *  This routine first clears the entire lval array (all fields are set to 0).
 *
 *  Global references: sc_intest  (may be altered, but restored upon termination)
 */
int
SC3ExpressionParser::primary(value *lval)
{
  char *st;
  int tok;
  cell val;
  symbol *sym;

  if (matchtoken('(')){         /* sub-expression - (expression,...) */
    PUSHSTK_I(sc_intest);
    PUSHSTK_I(sc_allowtags);

    sc_intest=FALSE;            /* no longer in "test" expression */
    sc_allowtags=TRUE;          /* allow tagnames to be used in parenthesized expressions */
    
    int lvalue = 0;
    int count = 0;
    do {
      lvalue = hier14(lval);
      count++;
    } while (matchtoken(','));
    needtoken(')');
    lexclr(FALSE);              /* clear lex() push-back, it should have been
                                 * cleared already by needtoken() */
    sc_allowtags=(short)POPSTK_I();
    sc_intest=(short)POPSTK_I();

    if (count > 1 && lvalue) {
      rvalue(lval);
      lvalue = FALSE;
    }
    return lvalue;
  } /* if */

  clear_value(lval);    /* clear lval */
  tok=lex(&val,&st);

  if (tok == tTHIS) {
    strcpy(lastsymbol, "this");
    if ((sym = findloc("this")) == NULL) {
      error(166);           /* 'this' outside method body */
      ldconst(0, sPRI);
      return FALSE;
    }
    
    assert(sym->ident == iVARIABLE);
    lval->sym = sym;
    lval->ident = sym->ident;
    lval->tag = sym->tag;
    return TRUE;
  }

  if (tok==tSYMBOL) {
    /* lastsymbol is char[sNAMEMAX+1], lex() should have truncated any symbol
     * to sNAMEMAX significant characters */
    assert(strlen(st)<sizeof lastsymbol);
    strcpy(lastsymbol,st);
  }
  if (tok==tSYMBOL && !findconst(st)) {
    /* first look for a local variable */
    if ((sym=findloc(st))!=0) {
      if (sym->ident==iLABEL) {
        error(29);          /* expression error, assumed 0 */
        ldconst(0,sPRI);    /* load 0 */
        return FALSE;       /* return 0 for labels (expression error) */
      } /* if */
      lval->sym=sym;
      lval->ident=sym->ident;
      lval->tag=sym->tag;
      if (sym->ident==iARRAY || sym->ident==iREFARRAY) {
        address(sym,sPRI);  /* get starting address in primary register */
        return FALSE;       /* return 0 for array (not lvalue) */
      } else {
        return TRUE;        /* return 1 if lvalue (not label or array) */
      } /* if */
    } /* if */
    /* now try a global variable */
    if ((sym = findglb(st)) != 0) {
      if (sym->ident==iFUNCTN) {
        /* if the function is only in the table because it was inserted as a
         * stub in the first pass (i.e. it was "used" but never declared or
         * implemented, issue an error
         */
        if ((sym->usage & uPROTOTYPED)==0)
          error(17,st);
      } else {
        if ((sym->usage & uDEFINE)==0)
          error(17,st);
        lval->sym=sym;
        lval->ident=sym->ident;
        lval->tag=sym->tag;
        switch (sym->ident) {
          case iARRAY:
          case iREFARRAY:
            address(sym,sPRI);    /* get starting address in primary register */
            return FALSE;         /* return 0 for array (not lvalue) */
          case iMETHODMAP:
            return FALSE;
          default:
            return TRUE;          /* return 1 if lvalue (not function or array) */
        } /* switch */
      } /* if */
    } else {
      // We assume this is a function that hasn't been seen yet. We should
      // either be in the first pass, or the second pass and skipping writes.
      // If we're writing, then this is an error.
      if (sc_status != statFIRST)
        return error(17, st);
      sym=fetchfunc(st);
      if (sym==NULL)
        error(FATAL_ERROR_OOM);
    } /* if */
    assert(sym!=NULL);
    assert(sym->ident==iFUNCTN);
    lval->sym=sym;
    lval->ident=sym->ident;
    lval->tag=sym->tag;
    return FALSE;       /* return 0 for function (not an lvalue) */
  } /* if */
  lexpush();            /* push the token, it is analyzed by constant() */
  if (constant(lval)==0) {
    error(29);          /* expression error, assumed 0 */
    ldconst(0,sPRI);    /* load 0 */
  } /* if */
  return FALSE;         /* return 0 for constants (or errors) */
}

static void clear_value(value *lval)
{
  lval->sym=NULL;
  lval->constval=0L;
  lval->tag=0;
  lval->ident=0;
  lval->boolresult=FALSE;
  lval->accessor=NULL;
}

static void setdefarray(cell *string,cell size,cell array_sz,cell *dataaddr,int fconst)
{
  /* The routine must copy the default array data onto the heap, as to avoid
   * that a function can change the default value. An optimization is that
   * the default array data is "dumped" into the data segment only once (on the
   * first use).
   */
  /* check whether to dump the default array */
  assert(dataaddr!=NULL);
  if (sc_status==statWRITE && *dataaddr<0) {
    int i;
    *dataaddr=(litidx+glb_declared)*sizeof(cell);
    for (i=0; i<size; i++)
      litadd(*string++);
  } /* if */

  /* if the function is known not to modify the array (meaning that it also
   * does not modify the default value), directly pass the address of the
   * array in the data segment.
   */
  if (fconst || !string) {
    ldconst(*dataaddr,sPRI);
  } else {
    /* Generate the code:
     *  CONST.pri dataaddr                ;address of the default array data
     *  HEAP      array_sz*sizeof(cell)   ;heap address in ALT
     *  MOVS      size*sizeof(cell)       ;copy data from PRI to ALT
     *  MOVE.PRI                          ;PRI = address on the heap
     */
    ldconst(*dataaddr,sPRI);
    /* "array_sz" is the size of the argument (the value between the brackets
     * in the declaration), "size" is the size of the default array data.
     */
    assert(array_sz>=size);
    modheap((int)array_sz*sizeof(cell));
    markheap(MEMUSE_STATIC, array_sz);
    /* ??? should perhaps fill with zeros first */
    memcopy(size*sizeof(cell));
    moveto1();
  } /* if */
}

int checktag(int tag,int exprtag)
{
  int errcount = errnum;

  if (matchtag(tag,exprtag,MATCHTAG_COERCE|MATCHTAG_SILENT))
    return TRUE;    /* matching tag */

  // If matchtag() didn't error, report an error.
  if (errnum == errcount)
    error(213);

  return FALSE;       /* no tag matched */
}

enum {
  ARG_UNHANDLED,
  ARG_IGNORED,
  ARG_DONE,
};

static unsigned sCallNesting = 0;
static unsigned sCallStackUsage = 0;

class CallArgPusher
{
 public:
  CallArgPusher(const svalue* implicit_this)
   : has_this_value_(false),
     handling_this_(false),
     old_call_stack_usage_(sCallStackUsage)
  {
    sCallNesting++;

    if (implicit_this) {
      has_this_value_ = true;
      handling_this_ = true;
      this_value_ = *implicit_this;
    }
  }
  ~CallArgPusher() {
    sCallNesting--;
    sCallStackUsage = old_call_stack_usage_;
  }

  void prepare() {
    if (has_this_value_ && !this_value_.canRematerialize())
      evaluateAndSaveThis();
  }

  bool handling_this() const {
    return handling_this_;
  }
  const svalue& thisv() const {
    assert(has_this_value_);
    return this_value_;
  }
  bool handling_complex_this() const {
    return handling_this() && !this_value_.canRematerialize();
  }
  unsigned old_call_stack_usage() const {
    return old_call_stack_usage_;
  }

  void next_arg() {
    // If we're not handling |this|, or the |this| value is not complex, then
    // it has just been evaluated and needs to be pushed. (Otherwise, if the
    // current argument is a complex |this|, then it was already pushed during
    // prepare).
    if (!handling_this() || this_value_.canRematerialize()) {
      if (has_this_value_ && !this_value_.canRematerialize()) {
        // If we push the current evaluated argument, |this| will be in the
        // wrong position since we already pushed |this| earlier. To guarantee
        // it's always in the right spot, we pop |this|, push our new argument,
        // then re-push |this|.
        popreg(sALT);
        pushreg(sPRI);
        pushreg(sALT);
      } else {
        pushreg(sPRI);
      }
      markexpr(sPARM,NULL,0);   /* mark the end of a sub-expression */
      sCallStackUsage++;
    }

    handling_this_ = false;
  }

 private:
  // If we have an implicit |this|, and it requires evaluating an expression
  // to compute, then we have to save PRI across each argument. The reason is
  // that Pawn will evaluate arguments like so:
  //
  //    x[5].z(1, 2)
  //
  // Resolves to:
  //
  //  begin:
  //    load.s.pri  x
  //    add.pri.c   5 * sizeof(cell)
  //  marked:
  //    push.pri    ; this
  //    const.pri 1
  //    push.pri
  //    const.pri 2
  //    push.pri 2
  //  finished:
  //    push.c 3    ; argc
  //
  // The expressions from "marked" to "finished" are reversed, so that
  // arguments appear in order. Thus the final sequence is:
  //
  //  begin:
  //    load.s.pri  x
  //    add.pri.c   5 * sizeof(cell)
  //  marked:
  //    const.pri 2
  //    push.pri 2
  //    const.pri 1
  //    push.pri
  //    push.pri    ; this
  //  finished:
  //    push.c 3    ; argc
  //
  // To account for this, if |this| requires evaluation, we evaluate it ahead
  // of time and store it on the stack, so we can extract it again later.
  // Because there is no opcode to do this, we have to play hot potato like so:
  //
  //  begin:
  //    load.s.pri  x
  //    add.pri.c   5 * sizeof(cell)
  //  marked:
  //    const.pri 2
  //    pop.alt     ; pop |this|
  //    push.pri 2
  //    push.alt    ; re-push |this|
  //    const.pri 1
  //    pop.alt     ; pop |this|
  //    push.pri
  //    push.alt    ; re-push |this|
  //  finished:
  //    push.c 3    ; argc
  //
  void evaluateAndSaveThis() {
    // Compute an rvalue of |implicit_this|. Note that if it's an accessor,
    // this reduces it to an iEXPRESSION. That's ok. We don't touch it
    // otherwise though, so the type checking logic below basically acts the
    // same. We are careful to not double-evaluate however.
    if (this_value_.lvalue)
      rvalue(&this_value_);
    pushreg(sPRI);
    sCallStackUsage++;
  }

 private:
  bool has_this_value_;
  bool handling_this_;
  svalue this_value_;
  unsigned old_call_stack_usage_;
};

/*  callfunction
 *
 *  Generates code to call a function. This routine handles default arguments
 *  and positional as well as named parameters.
 */
void
SC3ExpressionParser::callfunction(symbol *sym, const svalue *aImplicitThis, value *lval_result, int matchparanthesis)
{
  int close,lvalue;
  int argpos;       /* index in the output stream (argpos==nargs if positional parameters) */
  int argidx=0;     /* index in "arginfo" list */
  int heapalloc=0;
  int namedparams=FALSE;
  value lval = {0};
  arginfo *arg;
  char arglist[SP_MAX_CALL_ARGUMENTS];
  constvalue arrayszlst = { NULL, "", 0, 0};/* array size list starts empty */
  constvalue taglst = { NULL, "", 0, 0};    /* tag list starts empty */
  symbol *symret;
  cell lexval;
  char *lexstr;
  const int firstArgOffset = aImplicitThis ? 1 : 0;

  assert(sym!=NULL);
  lval_result->ident=iEXPRESSION; /* preset, may be changed later */
  lval_result->constval=0;
  lval_result->tag=sym->tag;
  /* check whether this is a function that returns an array */
  symret=sym->array_return();
  assert(symret==NULL || symret->ident==iREFARRAY);
  if (symret!=NULL) {
    int retsize;
    /* allocate space on the heap for the array, and pass the pointer to the
     * reserved memory block as a hidden parameter
     */
    retsize=(int)array_totalsize(symret);
    assert(retsize>0);
    modheap(retsize*sizeof(cell));/* address is in ALT */
    pushreg(sALT);                /* pass ALT as the last (hidden) parameter */
    markheap(MEMUSE_STATIC, retsize);
    /* also mark the ident of the result as "array" */
    lval_result->ident=iREFARRAY;
    lval_result->sym=symret;
  } /* if */
  pushheaplist();

  // If we're calling a stock in the 2nd pass, and it was never defined as
  // read, then we're encountering some kind of compiler bug. If we're not
  // supposed to emit this code than the status should be statSKIP - so
  // we're generating code that will jump to the wrong address.
  if ((sym->usage & uSTOCK) &&
      !(sym->usage & uREAD) &&
      sc_status == statWRITE)
  {
    error(195, sym->name());
  }

  if ((sym->flags & flgDEPRECATED)!=0) {
    const char *ptr= sym->documentation.chars();
    error(234,sym->name(),ptr);   /* deprecated (probably a native function) */
  } /* if */

  CallArgPusher args(aImplicitThis);

  // Setup pre-evaluation stuff for |this|.
  args.prepare();

  /* run through the arguments */
  arg=&sym->function()->args[0];
  assert(arg!=NULL);
  stgmark(sSTARTREORDER);
  memset(arglist,ARG_UNHANDLED,sizeof arglist);
  if (matchparanthesis) {
    /* Opening brace was already parsed, if closing brace follows, this
     * call passes no parameters.
     */
    close=matchtoken(')');
  } else {
    /* When we find an end of line here, it may be a function call passing
     * no parameters, or it may be that the first parameter is on a line
     * below. But as a parameter can be anything, this is difficult to check.
     * The only simple check that we have is the use of "named parameters".
     */
    close=matchtoken(tTERM);
    if (close) {
      close=!matchtoken('.');
      if (!close)
        lexpush();                /* reset the '.' */
    } /* if */
  } /* if */

  unsigned nargs = 0;
  if (args.handling_this() || !close) {
    do {
      bool was_handling_this = args.handling_this();
      if (!args.handling_this() && matchtoken('.')) {
        namedparams=TRUE;
        if (!needtoken(tSYMBOL))
          break;
        tokeninfo(&lexval,&lexstr);
        argpos=findnamedarg(arg,lexstr);
        if (argpos<0) {
          error(17,lexstr);       /* undefined symbol */
          break;                  /* exit loop, argpos is invalid */
        } /* if */
        needtoken('=');
        argidx=argpos;
      } else {
        if (namedparams)
          error(44);   /* positional parameters must precede named parameters */
        argpos=nargs;
      } /* if */
      /* the number of arguments this was already checked at the declaration
       * of the function; check it again for functions with a variable
       * argument list
       */
      if (argpos>=SP_MAX_CALL_ARGUMENTS)
        error(45);                /* too many function arguments */
      stgmark((char)(sEXPRSTART+argpos));/* mark beginning of new expression in stage */
      if (arglist[argpos]!=ARG_UNHANDLED)
        error(58);                /* argument already set */
      if (!args.handling_this() && matchtoken('_')) {
        arglist[argpos]=ARG_IGNORED;  /* flag argument as "present, but ignored" */
        if (arg[argidx].ident==0 || arg[argidx].ident==iVARARGS) {
          error(92);             /* argument count mismatch */
        } else if (!arg[argidx].hasdefault) {
          error(34,nargs+1);      /* argument has no default value */
        } /* if */
        if (arg[argidx].ident!=0 && arg[argidx].ident!=iVARARGS)
          argidx++;
        /* The rest of the code to handle default values is at the bottom
         * of this routine where default values for unspecified parameters
         * are (also) handled. Note that above, the argument is flagged as
         * ARG_IGNORED.
         */
      } else {
        arglist[argpos]=ARG_DONE; /* flag argument as "present" */
        if (args.handling_this()) {
          lval = args.thisv().val;
          lvalue = args.thisv().lvalue;
        } else {
          lvalue = hier14(&lval);
          if (lvalue && lval.ident == iACCESSOR) {
            rvalue(&lval);
            lvalue = FALSE;
          }
        }
        switch (arg[argidx].ident) {
        case 0:
          /* On the first pass, we donm't have all of the parameter info.
           * However, use information must be marked anyway, otherwise vars
           * declared previously will be omitted in the second psas. See
           * SourceMod bug 4643.
           */
          error(92);             /* argument count mismatch */
          if (lval.sym)
            markusage(lval.sym, uREAD);
          break;
        case iVARARGS:
          // hier13() should filter non-methodmap functions, and |this| is
          // always an iVARIABLE.
          assert(!args.handling_this());

          /* always pass by reference */
          if (lval.ident==iVARIABLE || lval.ident==iREFERENCE) {
            assert(lval.sym!=NULL);
            if ((lval.sym->usage & uCONST)!=0 && (arg[argidx].usage & uCONST)==0) {
              /* treat a "const" variable passed to a function with a non-const
               * "variable argument list" as a constant here */
              if (!lvalue) {
                error(22);        /* need lvalue */
              } else {
                rvalue(&lval);    /* get value in PRI */
                setheap_pri();    /* address of the value on the heap in PRI */
                heapalloc+=markheap(MEMUSE_STATIC, 1);
                sCallStackUsage++;
              } /* if */
            } else if (lvalue) {
              address(lval.sym,sPRI);
            } else {
              setheap_pri();      /* address of the value on the heap in PRI */
              heapalloc+=markheap(MEMUSE_STATIC, 1);
              sCallStackUsage++;
            } /* if */
          } else if (lval.ident==iCONSTEXPR || lval.ident==iEXPRESSION) {
            /* allocate a cell on the heap and store the
             * value (already in PRI) there */
            setheap_pri();        /* address of the value on the heap in PRI */
            heapalloc+=markheap(MEMUSE_STATIC, 1);
            sCallStackUsage++;
          }
          /* ??? handle const array passed by reference */
          /* otherwise, the address is already in PRI */
          if (lval.sym!=NULL)
            markusage(lval.sym,uWRITTEN);
          if (!checktag_string(arg[argidx].tag, &lval)
              && !checktag(arg[argidx].tag,lval.tag))
            error(213);
          break;
        case iVARIABLE:
          if (lval.ident==iLABEL || lval.ident==iFUNCTN ||
              lval.ident==iARRAY || lval.ident==iREFARRAY)
          {
            error(35,argidx+1-firstArgOffset);   /* argument type mismatch */
          }

          if (lvalue) {
            // Note: do not load anything if the implicit this was pre-evaluted
            // for being too complex.
            if (!args.handling_complex_this())
              rvalue(&lval);        /* get value (direct or indirect) */
          }

          /* otherwise, the expression result is already in PRI */

          // Do not allow user operators to transform |this|.
          if (!args.handling_this())
            check_userop(NULL,lval.tag,arg[argidx].tag,2,NULL,&lval.tag);
          if (!checktag_string(arg[argidx].tag, &lval))
            checktag(arg[argidx].tag, lval.tag);
          if (lval.tag!=0)
            append_constval(&taglst,arg[argidx].name,lval.tag,0);
          argidx++;               /* argument done */
          break;
        case iREFERENCE:
          // hier13() should filter non-methodmap functions, and |this| is
          // always an iVARIABLE.
          assert(!args.handling_this());

          if (!lvalue)
            error(35,argidx+1-firstArgOffset);   /* argument type mismatch */
          if (lval.sym!=NULL && (lval.sym->usage & uCONST)!=0 && (arg[argidx].usage & uCONST)==0)
            error(35,argidx+1-firstArgOffset);   /* argument type mismatch */
          if (lval.ident==iVARIABLE || lval.ident==iREFERENCE) {
            if (lvalue) {
              assert(lval.sym!=NULL);
              address(lval.sym,sPRI);
            } else {
              setheap_pri();      /* address of the value on the heap in PRI */
              heapalloc+=markheap(MEMUSE_STATIC, 1);
              sCallStackUsage++;
            } /* if */
          } /* if */
          /* otherwise, the address is already in PRI */
          checktag(arg[argidx].tag,lval.tag);
          if (lval.tag!=0)
            append_constval(&taglst,arg[argidx].name,lval.tag,0);
          argidx++;               /* argument done */
          if (lval.sym!=NULL)
            markusage(lval.sym,uWRITTEN);
          break;
        case iREFARRAY:
          // hier13() should filter non-methodmap functions, and |this| is
          // always an iVARIABLE.
          assert(!args.handling_this());

          if (lval.ident!=iARRAY && lval.ident!=iREFARRAY
              && lval.ident!=iARRAYCELL && lval.ident!=iARRAYCHAR)
          {
            error(35,argidx+1-firstArgOffset);   /* argument type mismatch */
            break;
          } /* if */
          if (lval.sym!=NULL && (lval.sym->usage & uCONST)!=0 && (arg[argidx].usage & uCONST)==0)
            error(35,argidx+1); /* argument type mismatch */
          /* Verify that the dimensions match with those in arg[argidx].
           * A literal array always has a single dimension.
           * An iARRAYCELL parameter is also assumed to have a single dimension.
           */
          if (lval.sym==NULL || lval.ident==iARRAYCELL || lval.ident==iARRAYCHAR) {
            if (arg[argidx].numdim!=1) {
              error(48);        /* array dimensions must match */
            } else if (arg[argidx].dim[0]!=0) {
              assert(arg[argidx].dim[0]>0);
              if (lval.ident==iARRAYCELL) {
                if (lval.constval==0 || arg[argidx].dim[0]!=lval.constval)
                  error(47);        /* array sizes must match */
              } else {
                assert(lval.constval!=0); /* literal array must have a size */
                /* A literal array must have exactly the same size as the
                 * function argument; a literal string may be smaller than
                 * the function argument.
                 */
                if ((lval.constval>0 && arg[argidx].dim[0]!=lval.constval)
                    || (lval.constval<0 && arg[argidx].dim[0] < -lval.constval))
                  error(47);      /* array sizes must match */
              } /* if */
            } /* if */
            if ((lval.ident!=iARRAYCELL && lval.ident!=iARRAYCHAR) || lval.constval>0) {
              /* save array size, for default values with uSIZEOF flag */
              cell array_sz=lval.constval;
              assert(array_sz!=0);/* literal array must have a size */
              if (array_sz<0)
                array_sz= -array_sz;
              append_constval(&arrayszlst,arg[argidx].name,array_sz,0);
            }/* if */
          } else {
            symbol *sym=lval.sym;
            short level=0;
            assert(sym!=NULL);
            if (sym->dim.array.level+1!=arg[argidx].numdim)
              error(48);          /* array dimensions must match */
            /* the lengths for all dimensions must match, unless the dimension
             * length was defined at zero (which means "undefined")
             */
            while (sym->dim.array.level>0) {
              assert(level<sDIMEN_MAX);
              if (arg[argidx].dim[level]!=0 && sym->dim.array.length!=arg[argidx].dim[level])
                error(47);        /* array sizes must match */
              else if (!matchtag(arg[argidx].idxtag[level],sym->x.tags.index,TRUE))
                error(229,sym->name());   /* index tag mismatch */
              append_constval(&arrayszlst,arg[argidx].name,sym->dim.array.length,level);
              sym=sym->array_child();
              assert(sym!=NULL);
              level++;
            } /* if */
            /* the last dimension is checked too, again, unless it is zero */
            assert(level<sDIMEN_MAX);
            assert(sym!=NULL);
            if (arg[argidx].dim[level]!=0 && sym->dim.array.length!=arg[argidx].dim[level])
              error(47);          /* array sizes must match */
            else if (!matchtag(arg[argidx].idxtag[level],sym->x.tags.index,TRUE))
              error(229,sym->name());   /* index tag mismatch */
            append_constval(&arrayszlst,arg[argidx].name,sym->dim.array.length,level);
          } /* if */
          /* address already in PRI */

          checktag(arg[argidx].tag,lval.tag);

          if ((arg[argidx].tag != pc_tag_string && lval.tag == pc_tag_string) ||
              (arg[argidx].tag == pc_tag_string && lval.tag != pc_tag_string))
          {
            error(178, type_to_name(lval.tag), type_to_name(arg[argidx].tag));
          }

          if (lval.tag!=0)
            append_constval(&taglst,arg[argidx].name,lval.tag,0);
          // ??? set uWRITTEN?
          argidx++;               /* argument done */
          break;
        } /* switch */

        args.next_arg();
      } /* if */
      assert(arglist[argpos]!=ARG_UNHANDLED);
      nargs++;

      if (was_handling_this && close)
        break;

      if (matchparanthesis) {
        close=matchtoken(')');
        if (!close)               /* if not paranthese... */
          /* Not expecting comma if the first argument was implicit. */
          if (!was_handling_this && !needtoken(','))
            break;                /* ...but abort loop if neither */
      } else {
        /* Not expecting comma if the first argument was implicit. */
        close = (!was_handling_this && !matchtoken(','));
        if (close) {              /* if not comma... */
          if (needtoken(tTERM)==1)/* ...must be end of statement */
            lexpush();            /* push again, because end of statement is analised later */
        } /* if */
      } /* if */

    } while (!close && freading && !matchtoken(tENDEXPR)); /* do */
  } /* if */
  /* check remaining function arguments (they may have default values) */
  for (argidx=0; arg[argidx].ident!=0 && arg[argidx].ident!=iVARARGS; argidx++) {
    if (arglist[argidx]==ARG_DONE)
      continue;                 /* already seen and handled this argument */
    stgmark((char)(sEXPRSTART+argidx));/* mark beginning of new expression in stage */
    if (arg[argidx].hasdefault) {
      if (arg[argidx].ident==iREFARRAY) {
        short level;
        setdefarray(arg[argidx].defvalue.array.data,
                    arg[argidx].defvalue.array.size,
                    arg[argidx].defvalue.array.arraysize,
                    &arg[argidx].defvalue.array.addr,
                    (arg[argidx].usage & uCONST)!=0);
        if (arg[argidx].defvalue.array.data != NULL) {
          if ((arg[argidx].usage & uCONST)==0) {
            heapalloc+=arg[argidx].defvalue.array.arraysize;
            sCallStackUsage+=arg[argidx].defvalue.array.arraysize;
          } /* if */
          /* keep the lengths of all dimensions of a multi-dimensional default array */
          assert(arg[argidx].numdim>0);
          if (arg[argidx].numdim==1) {
            append_constval(&arrayszlst,arg[argidx].name,arg[argidx].defvalue.array.arraysize,0);
          } else {
            for (level=0; level<arg[argidx].numdim; level++) {
              assert(level<sDIMEN_MAX);
              append_constval(&arrayszlst,arg[argidx].name,arg[argidx].dim[level],level);
            } /* for */
          } /* if */
        }
      } else if (arg[argidx].ident==iREFERENCE) {
        setheap(arg[argidx].defvalue.val);
        /* address of the value on the heap in PRI */
        heapalloc+=markheap(MEMUSE_STATIC, 1);
        sCallStackUsage++;
      } else {
        int dummytag=arg[argidx].tag;
        ldconst(arg[argidx].defvalue.val,sPRI);
        check_userop(NULL,arg[argidx].defvalue_tag,arg[argidx].tag,2,NULL,&dummytag);
        assert(dummytag==arg[argidx].tag);
      } /* if */
      args.next_arg();
    } else {
      error(92,argidx);        /* argument count mismatch */
    } /* if */
    if (arglist[argidx]==ARG_UNHANDLED)
      nargs++;
    arglist[argidx]=ARG_DONE;
  } /* for */
  stgmark(sENDREORDER);         /* mark end of reversed evaluation */

  sCallStackUsage++;
  ffcall(sym,NULL,nargs);
  if (sc_status!=statSKIP)
    markusage(sym,uREAD);       /* do not mark as "used" when this call itself is skipped */
  if (symret!=NULL)
    popreg(sPRI);               /* pop hidden parameter as function result */
  sideeffect=TRUE;              /* assume functions carry out a side-effect */
  delete_consttable(&arrayszlst);     /* clear list of array sizes */
  delete_consttable(&taglst);   /* clear list of parameter tags */

  /* maintain max. amount of memory used */
  {
    long totalsize;
    totalsize=declared+heapalloc+1;   /* local variables & return value size,
                                       * +1 for PROC opcode */
    if (lval_result->ident==iREFARRAY)
      totalsize++;                    /* add hidden parameter (on the stack) */
    if ((sym->usage & uNATIVE)==0)
      totalsize++;                    /* add "call" opcode */
    totalsize+=sCallStackUsage;
    if (curfunc != NULL) {
      if (curfunc->function()->stacksize<totalsize)
        curfunc->function()->stacksize=totalsize;
    } else {
      error(10);
    }
    // stack/heap space, +1 for argcount param. Ignore if we have errors.
    assert((sc_status != statWRITE || sc_total_errors) ||
           args.old_call_stack_usage() == sCallStackUsage - (nargs + heapalloc + 1));
  }

  /* scrap any arrays left on the heap, with the exception of the array that
   * this function has as a result (in other words, scrap all arrays on the
   * heap that caused by expressions in the function arguments)
   */
  popheaplist(true);
}

/*  dbltest
 *
 *  Returns a non-zero value if lval1 an array and lval2 is not an array and
 *  the operation is addition or subtraction.
 *
 *  Returns the "shift" count (1 for 16-bit, 2 for 32-bit) to align a cell
 *  to an array offset.
 */
static int dbltest(void (*oper)(),value *lval1,value *lval2)
{
  if ((oper!=ob_add) && (oper!=ob_sub))
    return 0;
  if (lval1->ident!=iARRAY)
    return 0;
  if (lval2->ident==iARRAY)
    return 0;
  return sizeof(cell)/2;        /* 1 for 16-bit, 2 for 32-bit */
}

/*  commutative
 *
 *  Test whether an operator is commutative, i.e. x oper y == y oper x.
 *  Commutative operators are: +  (addition)
 *                             *  (multiplication)
 *                             == (equality)
 *                             != (inequality)
 *                             &  (bitwise and)
 *                             ^  (bitwise xor)
 *                             |  (bitwise or)
 *
 *  If in an expression, code for the left operand has been generated and
 *  the right operand is a constant and the operator is commutative, the
 *  precautionary "push" of the primary register is scrapped and the constant
 *  is read into the secondary register immediately.
 */
static int commutative(void (*oper)())
{
  return oper==ob_add || oper==os_mult
         || oper==ob_eq || oper==ob_ne
         || oper==ob_and || oper==ob_xor || oper==ob_or;
}

/*  constant
 *
 *  Generates code to fetch a number, a literal character (which is returned
 *  by lex() as a number as well) or a literal string (lex() stores the
 *  strings in the literal queue). If the operand was a number, it is stored
 *  in lval->constval.
 *
 *  The function returns 1 if the token was a constant or a string, 0
 *  otherwise.
 */
static int constant(value *lval)
{
  int tok,index,ident;
  cell val,item,cidx;
  char *st;
  symbol *sym;

  tok=lex(&val,&st);
  if (tok==tSYMBOL && (sym=findconst(st))!=0) {
    lval->constval=sym->addr();
    ldconst(lval->constval,sPRI);
    lval->ident=iCONSTEXPR;
    lval->tag=sym->tag;
    lval->sym=sym;
    markusage(sym,uREAD);
  } else if (tok==tNULL) {
    lval->constval = 0;
    ldconst(lval->constval, sPRI);
    lval->ident = iCONSTEXPR;
    lval->tag = pc_tag_null_t;
  } else if (tok==tNUMBER) {
    lval->constval=val;
    ldconst(lval->constval,sPRI);
    lval->ident=iCONSTEXPR;
  } else if (tok==tRATIONAL) {
    lval->constval=val;
    ldconst(lval->constval,sPRI);
    lval->ident=iCONSTEXPR;
    lval->tag=sc_rationaltag;
  } else if (tok==tSTRING) {
    /* lex() stores starting index of string in the literal table in 'val' */
    ldconst((val+glb_declared)*sizeof(cell),sPRI);
    lval->ident=iARRAY;         /* pretend this is a global array */
    lval->constval=val-litidx;  /* constval == the negative value of the
                                 * size of the literal array; using a negative
                                 * value distinguishes between literal arrays
                                 * and literal strings (this was done for
                                 * array assignment). */
    lval->tag=pc_tag_string;
  } else if (tok=='{') {
    int tag,lasttag=-1;
    val=litidx;
    do {
      /* cannot call constexpr() here, because "staging" is already turned
       * on at this point */
      assert(staging);
      stgget(&index,&cidx);     /* mark position in code generator */
      ident=expression(&item,&tag,NULL,FALSE,NULL);
      stgdel(index,cidx);       /* scratch generated code */
      if (ident!=iCONSTEXPR)
        error(8);               /* must be constant expression */
      if (lasttag<0)
        lasttag=tag;
      else
        matchtag(lasttag,tag,FALSE);
      litadd(item);             /* store expression result in literal table */
    } while (matchtoken(','));
    if (!needtoken('}'))
      lexclr(FALSE);
    ldconst((val+glb_declared)*sizeof(cell),sPRI);
    lval->ident=iARRAY;         /* pretend this is a global array */
    lval->constval=litidx-val;  /* constval == the size of the literal array */
  } else {
    return FALSE;               /* no, it cannot be interpreted as a constant */
  } /* if */
  return TRUE;                  /* yes, it was a constant value */
}

