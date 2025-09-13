<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    require __DIR__ . '/type_helpers.php';
    
    $Data = json_decode($PageType['data'], true);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li>Types</li>
    <li class="active"><?php echo htmlspecialchars($PageType['name']); ?></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<h1 class="page-header"><?php
    echo htmlspecialchars($PageType['name']);
    if ($PageType['kind'] === 'typeset') {
        echo " TypeSet";
    } else if ($PageType['kind'] === 'typedef') {
        echo " Typedef";
    }
?>
</h1>

<?php if (empty($PageType['brief'])) { ?>
  <p class="text-muted">This type has no description.</p>
<?php } else { RenderDescription($PageType['brief']); } ?>

<?php
if ($PageType['kind'] === 'typeset')
{
    foreach ($Data['types'] as $TypeInfo)
    {
        echo '<div class="bs-callout bs-callout-info">';
        
        echo '<pre class="syntax">' . HighlightTypes($TypeInfo['type'], $TypeInfo['tags'], []) . '</pre>';
        
        if (empty($TypeInfo['brief']))
        {
            echo '<p class="text-muted">This type has no description.</p>';
        }
        else
        {
            RenderDescription($TypeInfo['brief']);
        }
        
        echo '<h4 class="sub-header2">Parameters</h4>';
        
        if (isset($TypeInfo['tags']))
        {
            RenderShortArgs($TypeInfo['tags']);
            
            // TODO: this is just a temporary hack
            // TODO: comment tags are not properly parsed for types (cc @dvander)
            foreach ($TypeInfo['tags'] as $Tag)
            {
                if (strncmp($Tag[0], "param:", 6) === 0)
                {
                    continue;
                }
                
                echo '<div class="bs-callout bs-callout-danger" style="padding:0;border:0"><h4>' . htmlspecialchars($Tag[0]) . '</h4>';
                RenderDescription($Tag[1]);
                echo '</div>';
            }
        }
        
        echo '</div>';
    }
}
else if ($PageType['kind'] === 'typedef') {
?>

<pre class="syntax"><span class="type">typedef</span> <?php echo $PageType['name'] . ' = ' . HighlightTypes($Data['type'], $Data['tags'], []); ?></pre>

<?php if (isset($Data['tags'])) { ?>
<h4 class="sub-header2">Tags</h4>
<dl>
<?php RenderShortArgs($Data['tags']); ?>
</dl>
<?php } ?>

<?php
}

if (isset($Data['tags']))
{
    RenderTags($Data['tags']);
}

?>

<?php
    require __DIR__ . '/footer.php';
?>
