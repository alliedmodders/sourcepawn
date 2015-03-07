<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
    $Data = json_decode($PageClass['data'], true);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars($PageClass['name']); ?> Class</h1>

<p><?php RenderDescription($PageClass['brief']); ?></p>

<?php
if ($PageClass['methods']) {
?>
    <h4 class="page-header">Methods</h4>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach ($PageClass['methods'] as $Method) {
                    $URL = $BaseURL .
                        $CurrentOpenFile .'/' .
                        htmlspecialchars($PageClass['name']) . '/' .
                        htmlspecialchars($Method['name']);
                    echo '<tr><td>';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Method['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Method['brief']);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageClass['properties']) {
?>
    <h4 class="page-header">Properties</h4>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Name</th>
                    <th>Type</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach ($PageClass['properties'] as $Property) {
                    $URL = $BaseURL .
                        $CurrentOpenFile .'/' .
                        htmlspecialchars($PageClass['name']) . '/' .
                        htmlspecialchars($Property['name']);
                    echo '<tr><td>';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Property['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>' . htmlspecialchars($Property['type']) . '</td>';
                    echo '<td>';
                    RenderDescription($Property['brief']);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
    require __DIR__ . '/footer.php';
?>
