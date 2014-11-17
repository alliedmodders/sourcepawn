// vim: set ts=2 sw=2 tw=99 et:
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
#ifndef _include_spcomp_source_location_h_
#define _include_spcomp_source_location_h_

namespace ke {

class FileContext;

// An encoded referece to a location in a source file. We keep this structure
// as small as feasible since our average script can have hundreds of thousands
// of source locations.
struct SourceLocation
{
  SourceLocation()
  {
  }

  bool isSet() const {
    return false;
  }
};

struct SourceRange
{
  SourceLocation start;
  SourceLocation end;

  SourceRange(const SourceLocation &start, const SourceLocation &end)
   : start(start),
     end(end)
  {}
};

}

#endif // _include_spcomp_source_location_h_
