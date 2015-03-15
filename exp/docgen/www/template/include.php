<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
    $FoundAny = false;
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li class="active">Overview</li>
        
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<?php
if ($PageClasses) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Classes <span class="badge"><?php echo count($PageClasses); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Class</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageClasses as $Class) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Class['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Class['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Class['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageEnums) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Enums <span class="badge"><?php echo count($PageEnums); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Enum</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageEnums as $Enum) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Enum['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Enum['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Enum['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageTypes) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Types <span class="badge"><?php echo count($PageTypes); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Type</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageTypes as $Type) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Type['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Type['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Type['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageConstants) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Constants <span class="badge"><?php echo count($PageConstants); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Constants</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageConstants as $Constant) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Constant['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Constant['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Constant['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageForwards) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Forwards <span class="badge"><?php echo count($PageForwards); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Function</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageForwards as $Function) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Function['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Function['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Function['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}
?>

<?php
if ($PageFunctions) {
    $FoundAny = true;
?>
    <h3 class="sub-header2">Functions <span class="badge"><?php echo count($PageFunctions); ?></span></h3>
    
    <div class="table-responsive">
        <table class="table table-bordered table-hover">
            <thead>
                <tr>
                    <th>Function</th>
                    <th>Description</th>
                </tr>
            </thead>
            <?php
                foreach($PageFunctions as $Function) {
                    $URL = $BaseURL . $CurrentOpenFile . '/' . htmlspecialchars($Function['name']);
                    echo '<tr><td class="col-md-2 mono">';
                    echo '<a href="' . $URL . '">' .
                         htmlspecialchars($Function['name']) .
                         '</a>';
                    echo '</td>';
                    echo '<td>';
                    RenderDescription($Function['brief'], true);
                    echo '</td></tr>';
                }
            ?>
        </table>
    </div>
<?php
}

if (!$FoundAny) {
?>
  <div class="bs-callout bs-callout-danger">
    <h4>This file is empty</h4>
    <p>This include file has no types, functions, or constants that we could display. Sorry about that.</p>
    <p>&nbsp;</p>
    <p><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">Would you like to view raw contents of this file?</a></p>
  </div>
<?php
}
?>

<?php
    require __DIR__ . '/footer.php';
?>
