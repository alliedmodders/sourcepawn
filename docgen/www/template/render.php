<?php
// vim: set ts=4 sw=4 tw=99 et:

function RenderDescription($Desc)
{
    $Lines = explode("\n", $Desc);
    foreach ($Lines as $Line) {
        echo "<p>" . htmlspecialchars($Line); "</p>";
    }
}

function RenderTags($Tags)
{
    $Notes = [];
    foreach ($Data['tags'] as $Tag) {
      if ($Tag['tag'] === 'note')
          $Notes[] = $Tag['text'];
    }

    if (!empty($Notes)) {
      echo '<h4 class="sub-header2">Notes</h4>';
      foreach ($Notes as $Note)
          RenderDescription($Note);
    }
}

function MethodName($Function)
{
    if (isset($Function['class_name'])) {
        return $Function['class_name'] . '.' . $Function['name'];
    }
    return $Function['name'];
}

?>
