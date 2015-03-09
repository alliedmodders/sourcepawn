<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
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

<p><?php RenderDescription($PageType['brief']); ?></p>

<?php
if ($PageType['kind'] === 'typeset') {
?>
    <h4 class="page-header">Possible Types:</h4>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Type</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach ($Data['types'] as &$TypeInfo) {
					echo '<tr>';
					echo '<td>' . htmlspecialchars($TypeInfo['type']) . '</td>';
                    echo '<td>';
                    RenderDescription($TypeInfo['brief']);
                    echo '<p>';
                    if (isset($TypeInfo['tags'])) {
                        RenderShortArgs($TypeInfo['tags']);
                    }
                    echo '</p>';
					echo '</td>';
					echo '</tr>';
                }
            ?>
        </table>
    </div>
<?php
} else if ($PageType['kind'] === 'typedef') {
?>
<?php
    echo '<p>';
    echo '<pre class="syntax">typedef ' . $PageType['name'] . ' = ' . $Data['type'] . ';</pre>';
    echo '</p>';
    if (isset($Data['tags'])) {
        echo '<p>';
        RenderShortArgs($Data['tags']);
        echo '</p>';
    }
}

if (isset($Data['tags'])) {
	RenderTags($Data['tags']);
}

?>

<?php
    require __DIR__ . '/footer.php';
?>
