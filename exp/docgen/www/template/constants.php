<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li class="active">Constants</li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<h1 class="page-header">List of constants in <?php echo htmlspecialchars( $PageName ); ?>.inc</h1>

<?php
    $InSection = 0;
    $InSectionBody = 0;
    
    foreach ($Results as $Result) {
        $Data = json_decode($Result['data'], true );
        $Slug = StringToSlug($Result['name']);

        echo '<div class="panel panel-primary" id="' . $Slug . '">';
        echo '<div class="panel-heading">' . htmlspecialchars($Result['brief']) . '<a href="#' . $Slug . '" class="permalink pull-right">#</a></div>';
            
        if (!Empty($Data['tags'])) {
            echo '<div class="panel-body">';
            foreach ($Tags as $Tag) {
                echo '<h4 class="sub-header2">' . ucfirst($Tag[0]) . '</h4>';
                
                if (isset($Tag[1])) {
                    echo '<pre class="description">' . htmlspecialchars($Tag[1]) . '</pre>';
                }
            }
            echo '</div>';
        }

        echo '<div class="panel-footer"><pre class="description">' . htmlspecialchars($Result['name']) . '</pre></div>';
        echo '</div>';
    }
?>

<?php
    require __DIR__ . '/footer.php';
    
    function StringToSlug( $String )
    {
        $String = preg_replace( '/[^A-Za-z0-9-]+/', '-', $String );
        $String = trim( $String, "- \t\n\r\0\x0B" );
        
        return strtolower( $String );
    }
?>
