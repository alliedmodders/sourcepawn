<?php
	require __DIR__ . '/header.php';
?>

<ol class="breadcrumb">
	<li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
	<li class="active">Raw</li>
	
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__functions">Functions</a></li>
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>">Constants</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars( $CurrentOpenFile ); ?>.inc</h1>

<pre><?php echo htmlspecialchars( $PageFile[ 'Content' ] ); ?></pre>

<?php
	require __DIR__ . '/footer.php';
?>
