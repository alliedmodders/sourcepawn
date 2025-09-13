<?php
// vim: set ts=4 sw=4 tw=99 et:
    require __DIR__ . '/header.php';
    require __DIR__ . '/type_helpers.php';

    $Data = json_decode($PageFunction['data'], true);

    $Parameters = $Data['params'];
    $OtherTags = Array();

    $TypesToLookup = $Parameters;

    if (isset($Data['return']))
    {
        $TypesToLookup[] = ['type' => $Data['return']['type']];
    }

    if (!empty($TypesToLookup))
    {
        $Types = LookupTypes($TypesToLookup);
    }

    foreach ($Data['tags'] as $Tag)
        $OtherTags[] = $Tag;
?>

<ol class="breadcrumb">
    <li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
<?php
if (isset($PageFunction['class_name'])) {
?>
    <li>Classes</li>
    <li><a href="<?php echo htmlspecialchars($BaseURL . $CurrentOpenFile . '/' . $PageFunction['class_name']); ?>"><?php echo htmlspecialchars($PageFunction['class_name']); ?></a></li>
<?php
}
?>
    <li><?php echo (isset($PageFunction['class_name']) ? 'Methods' : ($PageFunction['kind'] === 'forward' ? 'Forwards' : 'Functions')); ?></li>
    <li class="active"><?php echo htmlspecialchars($PageFunction['name']); ?></li>

    <li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
</ol>

<?php
$FunctionName = MethodName($PageFunction);
if (isset($PageFunction['class_name'])) {
?>
    <h1 class="page-header"><?php echo htmlspecialchars($FunctionName); ?> Method</h1>
<?php } else if ($PageFunction['kind'] === 'forward') { ?>
    <h1 class="page-header"><?php echo htmlspecialchars($FunctionName); ?> Forward</h1>
<?php } else { ?>
    <h1 class="page-header"><?php echo htmlspecialchars($FunctionName); ?> Function</h1>
<?php } ?>

<?php if (empty($PageFunction['brief'])) { ?>
  <p class="text-muted">This function has no description.</p>
<?php } else { RenderDescription($PageFunction['brief']); } ?>

<?php
if (isset($Data['return'])) {
    $Type = NormalizeType($Data['return']['type']);

    echo '<pre class="syntax">';

    if (isset($Types[$Type]))
    {
        echo '<a href="' . $BaseURL . $Types[$Type] . '/' . $Type . '" class="type b">' . htmlspecialchars($Data['return']['type']) . '</a>';
    }
    else
    {
        echo '<span class="type b">' . htmlspecialchars($Data['return']['type']) . '</span>';
    }
    
    echo HighlightTypes(substr($PageFunction['signature'], strlen($Data['return']['type'])), $Parameters, $Types);
    echo '</pre>';
} else {
    echo '<pre class="syntax">' . HighlightTypes($PageFunction['signature'], $Parameters, []) . '</pre>';
}
?>

<?php if (!empty($Parameters)) { ?>
<h4 class="sub-header2">Parameters</h4>

<dl>
<?php
    foreach ($Parameters as $Param)
    {
        echo '<dt class="mono">';
        
        $Type = NormalizeType($Param['type']);
        
        if (isset($Types[$Type]))
        {
            echo '<a href="' . $BaseURL . $Types[$Type] . '/' . $Type . '" class="type">' . htmlspecialchars($Param['type']) . '</a> ';
        }
        else
        {
            echo '<span class="type">' . htmlspecialchars($Param['type']) . '</span> ';
        }
        
        echo htmlspecialchars($Param['name']);
        echo '</dt>';
        
        echo '<dd>';
        
        if (empty($Param['doc']))
        {
            echo '<i class="text-muted">No description.</i>';
        }
        else
        {
            RenderDescription($Param['doc']);
        }
        
        echo '</dd>';
    }
?>
</dl>
<?php } ?>

<?php
if (isset($Data['return'])) {
  echo '<div class="bs-callout bs-callout-info"><h4>Return Value</h4>';

  if (empty($Data['return']['doc'])) {
    echo '<p class="text-muted">No description.</p>';
  } else {
    RenderDescription($Data['return']['doc']);
  }

  echo '</div>';
}
if (isset($Data['error'])) {
    echo '<div class="bs-callout bs-callout-danger"><h4>Errors</h4>';
    RenderDescription($Data['error']);
    echo '</div>';
}
?>

<?php
RenderTags($OtherTags);
?>

<?php
    require __DIR__ . '/footer.php';
?>
