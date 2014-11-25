// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 AlliedModders LLC, David Anderson
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
#include "reporting.h"
#include "compile-context.h"

using namespace ke;

ReportManager::ReportManager(CompileContext &cc)
 : cc_(cc),
   fatal_error_(rmsg::none)
{
}

void
ReportingContext::reportFatal(rmsg::Id msg)
{
  // We always report fatal errors.
  cc_.reportFatal(msg);
}
