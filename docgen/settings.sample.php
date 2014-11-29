<?php
	$Config = Array(
		'ConnectionString' => 'mysql:host=127.0.0.1;dbname=sm;charset=utf8mb4',
		'User'             => 'root',
		'Password'         => ''
	);
	
	$Columns = Array(
		'Functions' => 'pawnfunctions',
		'Constants' => 'pawnconstants',
		'Files'     => 'pawnfiles'
	);
	
	$BaseURL = '/pawn-docgen/www/';
	$Project = 'SourceMod';
	
	$Database = @new PDO(
		$Config[ 'ConnectionString' ],
		$Config[ 'User' ],
		$Config[ 'Password' ],
		Array(
			PDO :: ATTR_TIMEOUT            => 1,
			PDO :: ATTR_ERRMODE            => PDO :: ERRMODE_EXCEPTION,
			PDO :: ATTR_DEFAULT_FETCH_MODE => PDO :: FETCH_ASSOC
		)
	);
	
	unset( $Config );
