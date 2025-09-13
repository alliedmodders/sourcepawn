<?php
    require __DIR__ . '/header.php';
?>

<h1 class="page-header">Welcome to the <?php echo $Project; ?> Scripting API Reference</h1>

<div class="bs-callout">
    <p>Enter a search term on the left to look for symbols in the <?php echo $Project; ?> include files.</p>
    <p>Alternately, click a file on the left to browse its functions/symbols or see its contents.</p>
</div>

<div class="bs-callout bs-callout-info">
    <h4>Looking for more information?</h4>
    <p>For more information, see the <a href="http://wiki.alliedmods.net/Category:<?php echo str_replace( ' ', '_', $Project ); ?>_Scripting"><?php echo $Project; ?> Scripting Wiki</a>, which contains tutorials on specific topics.</p>
</div>

<div class="bs-callout bs-callout-danger">
    <h4>Find a mistake or missing comment?</h4>
    <p>We will happily accept your corrections and additions on <a href="https://github.com/alliedmodders">GitHub</a>!</p>
</div>

<?php
    require __DIR__ . '/footer.php';
?>
