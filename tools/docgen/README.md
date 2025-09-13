docgen
======

Docgen requires PHP 5, PDO, MySQL, and the 'docparser' tool in this repository. It uses docparser to find comments for important source constructs in Pawn include files, then parses the comments for doxygen-style markup. The results are pushed into the MySQL database and can be viewed with the code in `www`.

`www` requires a web server that can route all requests to `index.php`. This is done automatically on Apache using the `.htaccess` file. If you use nginx, you have to add these rules on your config:

```nginx
location /pawn-docgen/www/ {
	index index.php;
	try_files $uri @pawn_docgen_rewrite;
}

location @pawn_docgen_rewrite {
	rewrite "^/pawn-docgen/www/(.*)" /pawn-docgen/www/index.php?path=$1 last;
}
```

The code to docgen is based on xPaw's Pawn Docgen tool: https://github.com/alliedmodders/pawn-docgen/
