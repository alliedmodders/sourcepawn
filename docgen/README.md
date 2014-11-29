docgen
======

Docgen requires PHP 5, PDO, MySQL, and the 'docparser' tool in this repository. It uses docparser to find comments for important source constructs in Pawn include files, then parses the comments for doxygen-style markup. The results are pushed into the MySQL database and can be viewed with the code in `www`.
