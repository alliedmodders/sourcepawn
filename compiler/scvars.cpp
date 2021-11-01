/*  Pawn compiler
 *
 *  Global (cross-module) variables.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
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
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "scvars.h"
#include "sc.h"

/*  global variables
 *
 *  All global variables that are shared amongst the compiler files are
 *  declared here.
 */
char sc_ctrlchar = CTRL_CHAR;              /* the control character (or escape character)*/
char sc_ctrlchar_org = CTRL_CHAR;          /* the default control character */
int sc_rationaltag = 0;              /* tag for rational numbers */
int sc_allowproccall = 0;            /* allow/detect tagnames in lex() */
short sc_is_utf8 = FALSE;            /* is this source file in UTF-8 encoding */

std::shared_ptr<SourceFile> inpf;      /* file read from (source or include) */
std::shared_ptr<SourceFile> inpf_org;  /* main source file */
