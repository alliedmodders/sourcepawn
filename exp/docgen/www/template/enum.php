<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
    $Data = json_decode($PageEnum['data'], true);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars($PageEnum['name']); ?> Enumeration</h1>

<p><?php RenderDescription($PageEnum['brief']); ?></p>

<?php
if ($PageEnum['entries']) {
?>
    <h4 class="page-header">Values</h4>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach ($PageEnum['entries'] as $EnumValue) {
					echo '<tr>';
					echo '<td>' . htmlspecialchars($EnumValue['name']) . '</td>';
                    echo '<td>';
                    RenderDescription($EnumValue['brief']);
					echo '</td>';
					echo '</tr>';
                }
            ?>
        </table>
    </div>
<?php
}

if (isset($Data['tags'])) {
	RenderTags($Data['tags']);
}

?>

<?php
    require __DIR__ . '/footer.php';
?>
