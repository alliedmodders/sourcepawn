<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    require __DIR__ . '/type_helpers.php';
    
    $Data = json_decode($PageProperty['data'], true);

    $Types = LookupTypes([$PageProperty]);
    $Type = NormalizeType($PageProperty['type']);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    <li>Classes</li>
    <li><a href="<?php echo htmlspecialchars($BaseURL . $CurrentOpenFile . '/' . $PageProperty['class_name']); ?>"><?php echo htmlspecialchars($PageProperty['class_name']); ?></a></li>
    <li>Properties</li>
    <li class="active"><?php echo htmlspecialchars($PageProperty['name']); ?></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<?php
$PropertyName = $PageProperty['class_name'] . '.' .
                $PageProperty['name'];
?>
<h1 class="page-header"><?php echo htmlspecialchars($PropertyName); ?> Property</h1>

<?php if (empty($PageProperty['brief'])) { ?>
  <p class="text-muted">This property has no description.</p>
<?php } else { RenderDescription($PageProperty['brief']); } ?>

<h4 class="sub-header2">Usage</h4>
<table class="table table-condensed table-bordered" style="width: auto;">
 <tr>
  <td><strong>Type</strong></td>
<?php if (isset($Types[$Type])) { ?>
  <td><a href="<?php echo $BaseURL . $Types[$Type] . '/' . $Type; ?>" class="type"><?php echo htmlspecialchars($PageProperty['type']); ?></a></td>
<?php } else { ?>
  <td class="type"><?php echo htmlspecialchars($PageProperty['type']); ?></td>
<?php } ?>
 </tr>
 <tr>
  <td><strong>Getter</strong></td>
  <td><?php echo ($PageProperty['getter'] ? "Yes" : "No"); ?></td>
 </tr>
 <tr>
  <td><strong>Setter</strong></td>
  <td><?php echo ($PageProperty['setter'] ? "Yes" : "No"); ?></td>
 </tr>
</table>

<?php
if (isset($PageProperty['data']['tags'])) {
    RenderTags($PageProperty['data']['tags']);
}
?>

<?php
    require __DIR__ . '/footer.php';
?>
