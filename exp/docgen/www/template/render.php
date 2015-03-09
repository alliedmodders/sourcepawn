<?php
// vim: set ts=4 sw=4 tw=99 et:

function RenderDescription($Desc)
{
    $Lines = explode("\n", $Desc);
    foreach ($Lines as $Line) {
        echo "<p>" . htmlspecialchars($Line) . "</p>";
    }
}

function RenderTags($Tags)
{
    $Notes = [];
    foreach ($Tags as $Tag) {
      if ($Tag['tag'] === 'note')
          $Notes[] = $Tag['text'];
    }

    if (!empty($Notes)) {
      echo '<h4 class="sub-header2">Notes</h4>';
      foreach ($Notes as $Note)
          RenderDescription($Note);
    }
}

function RenderShortArgs($Tags)
{
    foreach ($Tags as $Tag) {
      $tag_name = $Tag[0];
      $tag_value = $Tag[1];
      if (strncmp($tag_name, "param:", 6) != 0)
          continue;

      echo "<p>";
      echo "<strong>" . htmlspecialchars(substr($tag_name, 6)) . "</strong>";
      echo " - " . htmlspecialchars($tag_value);
      echo "</p>";
    }
}

function MethodName($Function)
{
    if (isset($Function['class_name'])) {
        return $Function['class_name'] . '.' . $Function['name'];
    }
    return $Function['name'];
}
