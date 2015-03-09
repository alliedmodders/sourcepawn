<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
    $Data = json_decode($PageFunction['data'], true);
    
    $Parameters = $Data['params'];
    $OtherTags = Array();
    
    foreach ($Data['tags'] as $Tag)
        $OtherTags[] = $Tag;
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
<?php
if (isset($PageFunction['class_name'])) {
?>
    <li><?php echo htmlspecialchars($PageFunction['class_name']); ?></li>
<?php
}
?>
    <li><?php echo (isset($PageFunction['class_name']) ? 'Methods' : ($PageFunction['kind'] === 'forward' ? 'Callbacks' : 'Functions')); ?></li>
    <li class="active"><?php echo htmlspecialchars($PageFunction['name']); ?></li>

    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<?php
$FunctionName = MethodName($PageFunction);
if (isset($PageFunction['class_name'])) {
?>
    <h1 class="page-header"><?php echo htmlspecialchars($FunctionName); ?> Method</h1>
<?php } else { ?>
    <h1 class="page-header"><?php echo htmlspecialchars($FunctionName); ?> Function</h1>
<?php } ?>

<h4 class="sub-header2">Syntax</h4>
<pre class="syntax"><?php echo htmlspecialchars($PageFunction['signature']); ?></pre>

<?php if (!empty($Parameters)): ?>
<h4 class="sub-header2">Usage</h4>
<div class="table-responsive">
    <table class="table table-condensed table-bordered">
        <?php
            foreach ($Parameters as $Param) {
                echo '<tr>' .
                     '<td width="15%"><pre class="description">' . htmlspecialchars($Param['name']) . '</pre></td>' .
                     '<td width="15%"><pre class="description">' . htmlspecialchars($Param['type']) . '</pre></td>' .
                     '<td width="70%">' . htmlspecialchars($Param['doc']) . '</td>' .
                     '</tr>';
            }
        ?>
    </table>
</div>
<?php endif; ?>

<h4 class="sub-header2">Description</h4>
<?php RenderDescription($PageFunction['brief']); ?>

<?php
if (isset($Data['return'])) {
  echo '<h4 class="sub-header2">Return Value</h4>';
  RenderDescription($Data['return']['doc']);
}
if (isset($Data['error'])) {
  echo '<h4 class="sub-header2">Errors</h4>';
  RenderDescription($Data['error']);
}
?>

<?php
RenderTags($OtherTags);
?>

<?php
    require __DIR__ . '/footer.php';
?>
