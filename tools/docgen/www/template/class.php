<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    require __DIR__ . '/type_helpers.php';
    
    $Data = json_decode($PageClass['data'], true);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li>Classes</li>
    <li class="active"><?php echo htmlspecialchars($PageClass['name']); ?></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars($PageClass['name']); ?> Class</h1>

<?php if (empty($PageClass['brief'])) { ?>
  <p class="text-muted">This class has no description.</p>
<?php } else { RenderDescription($PageClass['brief']); } ?>

<?php
if (!empty($PageClass['methods'])) {
?>
    <h4 class="sub-header2">Methods <span class="badge"><?php echo count($PageClass['methods']); ?></span></h4>
    
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
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Method['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Method['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if (!empty($PageClass['properties'])) {
    $Types = LookupTypes($PageClass['properties']);
?>
    <h4 class="sub-header2">Properties <span class="badge"><?php echo count($PageClass['properties']); ?></span></h4>
    
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
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Property['name']) .
                         '</a>';
                    echo '</td>';
                    
                    $Type = NormalizeType($Property['type']);
                    
                    if (isset($Types[$Type]))
                    {
                        echo '<td class="col-md-1 mono"><a href="' . $BaseURL . $Types[$Type] . '/' . $Type . '" class="type">' . htmlspecialchars($Property['type']) . '</a></td>';
                    }
                    else
                    {
                        echo '<td class="col-md-1 mono type">' . htmlspecialchars($Property['type']) . '</td>';
                    }
                    
                    echo '<td>';
                    RenderDescription($Property['brief'], true);
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
