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
#include "sctracker.h"
#include "scvars.h"

bool
EnumDecl::Bind()
{
    int tag = 0;
    if (label_) {
        if (pc_findtag(label_->chars()) == 0) {
            // No implicit-int allowed.
            error(pos_, 169);
            label_ = nullptr;
        } else {
            tag = gTypes.defineEnumTag(label_->chars())->tagid();
        }
    }

    if (name_) {
        if (label_)
            error(pos_, 168);
        tag = gTypes.defineEnumTag(name_->chars())->tagid();
    } else {
        // The name is automatically the label.
        name_ = label_;
    }

    if (tag) {
        auto spec = deduce_layout_spec_by_tag(tag);
        if (!can_redef_layout_spec(spec, Layout_Enum))
            error(pos_, 110, name_->chars(), layout_spec_name(spec));
    }

    symbol* enumsym = nullptr;
    constvalue* enumroot = nullptr;
    if (name_) {
        if (vclass_ == sGLOBAL) {
            if ((enumsym = findglb(name_->chars())) != NULL) {
                // If we were previously defined as a methodmap, don't overwrite the
                // symbol. Otherwise, flow into add_constant where we will error.
                if (enumsym->ident != iMETHODMAP)
                    enumsym = nullptr;
            }
        }

        if (!enumsym) {
            // create the root symbol, so the fields can have it as their "parent"
            enumsym = add_constant(name_->chars(), 0, vclass_, tag);
            if (enumsym)
                enumsym->enumroot = true;
            // start a new list for the element names
            if ((enumroot = (constvalue*)calloc(1, sizeof(constvalue))) == NULL)
                error(pos_, FATAL_ERROR_OOM); /* insufficient memory (fatal error) */
        }
    }

    // If this enum is for a methodmap, forget the symbol so code below doesn't
    // build an enum struct.
    if (enumsym && enumsym->ident == iMETHODMAP)
        enumsym = NULL;

    for (const auto& field : fields_ ) {
        if (findconst(field.name->chars()))
            error(field.pos, 50, field.name->chars());

        symbol* sym = add_constant(field.name->chars(), field.value, vclass_, tag);
        if (!sym)
            continue;

        // set the item tag and the item size, for use in indexing arrays
        sym->set_parent(enumsym);
        // add the constant to a separate list as well
        if (enumroot) {
            sym->enumfield = true;
            append_constval(enumroot, field.name->chars(), field.value, tag);
        }
    }

    // set the enum name to the "next" value (typically the last value plus one)
    if (enumsym) {
        assert(enumsym->enumroot);
        enumsym->setAddr(0);
        // assign the constant list
        assert(enumroot);
        enumsym->dim.enumlist = enumroot;
    }
    return true;
}

bool
PstructDecl::Bind()
{
    const char* name = name_->chars();
    auto spec = deduce_layout_spec_by_name(name);
    if (!can_redef_layout_spec(spec, Layout_PawnStruct)) {
        error(pos_, 110, name, layout_spec_name(spec));
        return false;
    }
    if (!isupper(*name)) {
        error(pos_, 109, "struct");
        return false;
    }

    pstruct_t* pstruct = pstructs_add(name);
    gTypes.definePStruct(pstruct->name, pstruct);

    for (const auto& field : fields_) {
        structarg_t arg;
        arg.tag = field.type.tag;
        arg.dimcount = field.type.numdim;
        memcpy(arg.dims, field.type.dim, sizeof(int) * arg.dimcount);
        ke::SafeStrcpy(arg.name, sizeof(arg.name), field.name->chars());
        arg.fconst = field.type.is_const;
        arg.ident = field.type.ident;
        if (arg.ident == iARRAY)
            arg.ident = iREFARRAY;

        if (!pstructs_addarg(pstruct, &arg)) {
            error(field.pos, 103, arg.name, layout_spec_name(Layout_PawnStruct));
            return false;
        }
    }
    return true;
}

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
    if (sc_status != statFIRST && sym_->ident == iFUNCTN && !sym_->prototyped) {
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
    if (sym && sym->ident == iFUNCTN && !sym->defined)
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
