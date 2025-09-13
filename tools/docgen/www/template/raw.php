<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li class="active">File</li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>">Overview</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars( $CurrentOpenFile ); ?>.inc</h1>

<pre><?php echo htmlspecialchars($PageFile['content']); ?></pre>

<?php
    require __DIR__ . '/footer.php';
?>
