// vim: set ts=8 sts=4 sw=4 tw=99 et:
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

#include <stdarg.h>

#include <amtl/am-string.h>
#include "lexer.h"
#include "sc.h"

enum class ErrorType { Suppressed, Warning, Error, Fatal };

struct ErrorReport {
    static ErrorReport infer_va(int number, va_list ap);
    static ErrorReport create_va(int number, int fileno, int lineno, va_list ap);

    int number;
    int fileno;
    int lineno;
    const char* filename;
    std::string message;
    ErrorType type;
};

enum FatalError {
    FIRST_FATAL_ERROR = 183,

    FATAL_ERROR_READ = FIRST_FATAL_ERROR,
    FATAL_ERROR_WRITE,
    FATAL_ERROR_ALLOC_OVERFLOW,
    FATAL_ERROR_OOM,
    FATAL_ERROR_INVALID_INSN,
    FATAL_ERROR_INT_OVERFLOW,
    FATAL_ERROR_SCRIPT_OVERFLOW,
    FATAL_ERROR_OVERWHELMED_BY_BAD,
    FATAL_ERROR_NO_CODEPAGE,
    FATAL_ERROR_INVALID_PATH,
    FATAL_ERROR_ASSERTION_FAILED,
    FATAL_ERROR_USER_ERROR,
    FATAL_ERROR_NO_GENERATED_CODE,
    FATAL_ERROR_FUNCENUM,

    FATAL_ERRORS_TOTAL
};

class AutoErrorPos final
{
  public:
    explicit AutoErrorPos(const token_pos_t& pos);
    ~AutoErrorPos();

    const token_pos_t& pos() const {
        return pos_;
    }

  private:
    token_pos_t pos_;
    AutoErrorPos* prev_;
};

int error(int number, ...);
int error(symbol* sym, int number, ...);
int error(const token_pos_t& where, int number, ...);
int error_va(const token_pos_t& where, int number, va_list ap);
void errorset(int code, int line);
void report_error(ErrorReport* report);

int pc_enablewarning(int number, int enable);

#endif // am_sourcepawn_compiler_sc5_h
