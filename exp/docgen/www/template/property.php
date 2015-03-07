<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    
    $Data = json_decode($PageProperty['data'], true);
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
    
    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<?php
$PropertyName = $PageProperty['class_name'] . '.' .
                $PageProperty['name'];
?>
<h1 class="page-header"><?php echo htmlspecialchars($PropertyName); ?> Property</h1>

<h4 class="sub-header2">Usage</h4>
<table class="table table-condensed table-bordered" style="width: auto;">
 <tr>
  <td><strong>Type:</strong></td>
  <td><?php echo htmlspecialchars($PageProperty['type']); ?></td>
 </tr>
 <tr>
  <td><strong>Getter:</strong></td>
  <td><?php echo ($PageProperty['getter'] ? "yes" : "no"); ?></td>
 </tr>
 <tr>
  <td><strong>Setter:</strong></td>
  <td><?php echo ($PageProperty['setter'] ? "yes" : "no"); ?></td>
 </tr>
</table>

<h4 class="sub-header2">Description</h4>
<?php
RenderDescription($PageProperty['brief']);

if (isset($PageProperty['data']['tags'])) {
    RenderTags($PageProperty['data']['tags']);
}
?>

<?php
    require __DIR__ . '/footer.php';
?>
