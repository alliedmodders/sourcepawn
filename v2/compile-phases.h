// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 David Anderson
//
// This file is part of SourcePawn.
//
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#ifndef _include_spcomp2_compile_phases_
#define _include_spcomp2_compile_phases_

#include "compile-context.h"

namespace sp {

// Create symbol and scope hierarchies, and bind all names to available
// definitions. If this succeeds, all NameProxies that need to be bound
// will have been bound.
//
// This phase is a single pass over the AST.
bool
ResolveNames(CompileContext &cc, TranslationUnit *unit);

// Resolve TypeSpecifiers for all nodes except ones that require advanced
// type deduction. If this succeeds, all TypeSpecifiers will be resolved
// to Type objects, and all Type objects will be populated.
//
// This phase is a single pass over the AST.
bool
ResolveTypes(CompileContext &cc, TranslationUnit *unit);

}

#endif // _include_spcomp2_compile_phases_
