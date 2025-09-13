<?php
// vim: set ts=4 sw=4 tw=99 et:

function RenderDescription($Desc, $FirstLineOnly = false)
{
    $Lines = explode("\n", $Desc);
    foreach ($Lines as $Line) {
        echo "<p>" . htmlspecialchars($Line) . "</p>";
        if ($FirstLineOnly)
        {
            break;
        }
    }
}

function RenderTags($Tags)
{
    $Notes = [];
    foreach ($Tags as $Tag)
    {
      if (isset($Tag['tag']) && $Tag['tag'] === 'note')
          $Notes[] = $Tag['text'];
    }

    if (!empty($Notes))
    {
      echo '<div class="bs-callout"><h4>Notes</h4>';
      foreach ($Notes as $Note)
      {
          RenderDescription($Note);
      }
      echo '</div>';
    }
}

function RenderShortArgs($Tags)
{
    foreach ($Tags as $Tag)
    {
        $TagName = $Tag[0];
        $TagValue = $Tag[1];
        
        if (strncmp($TagName, "param:", 6) !== 0)
        {
            continue;
        }
        
        echo '<dt class="mono">';
        echo htmlspecialchars(substr($TagName, 6));
        echo '</dt>';
        
        echo '<dd>';
        
        if (empty($TagValue))
        {
            echo '<i class="text-muted">No description.</i>';
        }
        else
        {
            RenderDescription($TagValue);
        }
        
        echo '</dd>';
    }
}

function MethodName($Function)
{
    if (isset($Function['class_name'])) {
        return $Function['class_name'] . '.' . $Function['name'];
    }
    return $Function['name'];
}
