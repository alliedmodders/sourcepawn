<?php
    header( $_SERVER[ 'SERVER_PROTOCOL' ] . ' 404 Not Found' );
    
    require __DIR__ . '/header.php';
?>

<h1>Not Found</h1>

<div class="bs-callout bs-callout-danger">
    <h4>404</h4>
    <p>Sorry, we couldn't find anything matching your request.</p>
    <p>&nbsp;</p>
    <p><a href="<?php echo $BaseURL; ?>">← Return to the homepage</a></p>
</div>

<?php
    require __DIR__ . '/footer.php';
?>
