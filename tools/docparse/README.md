tools/docparse
==============

Docparse is a simple tool to extract comment and source structure from a SourcePawn file. The results are returned as JSON. It currently returns the following markup:
 - Functions and their arguments and argument types.
 - Enumerations and their values.
 - Classes/methodmaps and their properties, methods, and fields.
 - Constants.

Comments are attached as ranges (`docStart` and `docEnd` properties in JSON). Multiple C or C++-style comments can be included in a comment range. The range is specified as a range `(docStart, docEnd]` offset into the source file. It is important that the file does not change in between generating and using these offsets, and that the file is read in binary mode (not text mode).

Docparse's major limitation is that it does not perform any semantic analysis. It even ignores `#include`. It also doesn't have token ranges, so it can't provide default values or constant/enum initializers. Eventually these problems will be addressed.
