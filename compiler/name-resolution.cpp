// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//
//  Version: $Id$

#include "errors.h"
#include "expressions.h"
#include "parse-node.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"

bool
SymbolExpr::Bind()
{
    AutoErrorPos aep(pos_);

    sym_ = findconst(name_->chars());
    if (!sym_)
        sym_ = findloc(name_->chars());
    if (!sym_)
        sym_ = findglb(name_->chars());

    if (!sym_) {
        // We assume this is a function that hasn't been seen yet. We should
        // either be in the first pass, or the second pass and skipping writes.
        // If we're writing, then this is an error.
        if (sc_status != statFIRST) {
            error(pos_, 17, name_->chars());
            return false;
        }

        sym_ = fetchfunc(name_->chars());
    }

    /* if the function is only in the table because it was inserted as a
     * stub in the first pass (i.e. it was "used" but never declared or
     * implemented, issue an error
     */
    if (sc_status != statFIRST && sym_->ident == iFUNCTN && (sym_->usage & uPROTOTYPED) == 0) {
        error(pos_, 17, name_->chars());
        return false;
    }

    // Aggressively mark all symbols as read. This is a workaround until the
    // two-pass system can be removed. In the old parser, it was necessary
    // to aggressively generate as much code as early as possible, because
    // marking global functions as read produced correct output in the next
    // pass. However, our new parser short-circuits during the analysis phase
    // if an error occurs, which means we won't have our normal opportunities
    // to call markusage().
    //
    // As a workaround, we always call markusage() during binding. Note that
    // we preserve some old behavior where functions are not marked if being
    // skipped.
    if (!(sym_->ident == iFUNCTN && sc_status == statSKIP))
        markusage(sym_, uREAD);
    return true;
}

bool
ThisExpr::Bind()
{
    AutoErrorPos aep(pos_);

    sym_ = findloc("this");
    if (!sym_) {
        error(pos_, 166);
        return false;
    }
    return true;
}

bool
CallExpr::Bind()
{
    AutoErrorPos aep(pos_);

    if (!target_->Bind())
        return false;

    bool ok = true;
    for (const auto& arg : args_) {
        if (arg.expr)
           ok &= arg.expr->Bind();
    }
    return ok;
}

bool
CommaExpr::Bind()
{
    AutoErrorPos aep(pos_);

    bool ok = true;
    for (const auto& expr : exprs_) {
       ok &= expr->Bind();
    }
    return ok;
}

bool
ArrayExpr::Bind()
{
    AutoErrorPos aep(pos_);

    bool ok = true;
    for (const auto& expr : exprs_) {
       ok &= expr->Bind();
    }
    return ok;
}

bool
IsDefinedExpr::Bind()
{
    AutoErrorPos aep(pos_);

    symbol* sym = findloc(name_->chars());
    if (!sym)
        sym = findglb(name_->chars());
    if (sym && sym->ident == iFUNCTN && (sym->usage & uDEFINE) == 0)
        sym = nullptr;
    value_ = sym ? 1 : 0;
    if (!value_ && find_subst(name_->chars(), name_->length(), nullptr))
        value_ = 1;
    return true;
}

bool
SizeofExpr::Bind()
{
    AutoErrorPos aep(pos_);

    sym_ = findloc(ident_->chars());
    if (!sym_)
        sym_ = findglb(ident_->chars());
    if (!sym_) {
        error(pos_, 17, ident_->chars());
        return false;
    }
    markusage(sym_, uREAD);
    return true;
}

bool
ChainedCompareExpr::Bind()
{
    AutoErrorPos aep(pos_);

    bool ok = first_->Bind();
    for (const auto& op : ops_)
        ok &= op.expr->Bind();
    return ok;
}
