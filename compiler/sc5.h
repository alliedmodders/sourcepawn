// vim: set ts=8 sts=2 sw=2 tw=99 et:
/*  Pawn compiler - Error message system
 *  In fact a very simple system, using only 'panic mode'.
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
#ifndef am_sourcepawn_compiler_sc5_h
#define am_sourcepawn_compiler_sc5_h

#include <amtl/am-string.h>

enum class ErrorType {
  Suppressed,
  Warning,
  Error,
  Fatal
};

struct ErrorReport
{
  static ErrorReport infer_va(int number, va_list ap);
  static ErrorReport create_va(int number,
                               int fileno,
                               int lineno,
                               va_list ap);

  int number;
  int fileno;
  int lineno;
  const char* filename;
  ke::AString message;
  ErrorType type;
};

void report_error(ErrorReport* report);

#endif // am_sourcepawn_compiler_sc5_h
