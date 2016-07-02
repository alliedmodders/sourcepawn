Test structure is as follows:
 - basic: Extremely simple funtional tests that do not test any particular bug or feature. Basic
   tests are designed to exercise every virtual machine opcode as simply as possible.

Tests may appear anywhere and will run as long as they end in ".sp".

Manifests
---------

The first lines of a script may be comments of the form "// key: value". These are directives that
control the test harness. Currently supported key/value pairs:
 - returnCode: Must be an integer. The return code of the shell must match this value.

Output Checking
---------------

Tests can print to stdout, and have the test framework check that its output matches an expected
string. To do this, place a file in the same folder as the test, with the same name except with
".out" as its extension (instead of ".sp"). The output should have the same number of lines. Lines
in the .out file may contain Windows or Unix-style line endings.

The last line is always fuzzy-matched. If the stdout of the shell contains an extra empty line, the
.out file does not also need to contain an extra empty line.
