<?php
    $AnythingRendered = false;

    foreach ($Includes as $File)
    {
        $NoObjects = empty($Functions[$File]) && empty($Classes[$File]) && empty($Types[$File]);

        if ($Search !== false && $NoObjects)
        {
            continue;
        }

        $AnythingRendered = true;

        echo '<h4><a class="file" data-file="' . $File . '" href="' . $BaseURL . $File . '">' . $File . '</a></h4>';
        echo '<div class="nav-functions ' . ( $CurrentOpenFile === '%all%' || $CurrentOpenFile === $File ? ' show' : '' ) . '" id="file-' . $File . '">';

        if (!empty($Classes[$File])) {
            echo GetTypeHeader('class') . '<div class="panel-body panel-sidebar"><ul class="nav nav-sidebar">';
            foreach ($Classes[$File] as $Class) {
                $ClassName = htmlspecialchars($Class['Name']);
                $ClassBrief = htmlspecialchars($Class['Comment']);
                
                echo '<li class="function' .
                    ($CurrentOpenObject === $ClassName ? ' active' : '') .
                    '" data-title="' . $ClassName . '" data-content="' . $ClassBrief . '">';
                echo '<a href="' . $BaseURL . $File . '/' . urlencode($Class['Name']) . '">' . $ClassName . '</a>';
                echo '</li>';
            }
            
            echo '</ul></div></div>';
        }

        if (!empty($Types[$File])) {
            echo GetTypeHeader('enum') . '<div class="panel-body panel-sidebar"><ul class="nav nav-sidebar">';
            foreach ($Types[$File] as $Type) {
                $TypeName = htmlspecialchars($Type['Name']);
                $TypeBrief = htmlspecialchars($Type['Comment']);
                
                echo '<li class="function' .
                    ($CurrentOpenObject === $TypeName ? ' active' : '') .
                    '" data-title="' . $TypeName . '" data-content="' . $TypeBrief . '">';
                echo '<a href="' . $BaseURL . $File . '/' . urlencode($Type['Name']) . '">' . $TypeName . '</a>';
                echo '</li>';
            }
            
            echo '</ul></div></div>';
        }
        
        if (!empty($Functions[$File])) {
            $PreviousFunctionType = 'hypehypehype';
            $OpenList = false;
            
            foreach ($Functions[$File] as $Function) {
                if ($PreviousFunctionType !== $Function['Type']) {
                    $PreviousFunctionType = $Function['Type'];
                    
                    if ($OpenList) {
                        echo '</ul></div></div>';
                    }
                    
                    $OpenList = true;
                    
                    echo GetTypeHeader($Function['Type']) . '<div class="panel-body panel-sidebar"><ul class="nav nav-sidebar">';
                }
                
                $FunctionName = htmlspecialchars($Function['Function']);
                
                echo '<li class="function' .
                    ($CurrentOpenObject === $FunctionName ? ' active' : '') .
                    '" data-title="' . $FunctionName. '" data-content="' .
                    htmlspecialchars($Function['Comment']) . '">';
                echo '<a href="' . $BaseURL . $File . '/' . urlencode($Function['Function']) . '">' . $FunctionName . '</a>';
                echo '</li>';
            }
            
            if ($OpenList) {
                echo '</ul></div></div>';
            }
        }

        if ($NoObjects) {
            echo '<div class="panel panel-primary"><div class="panel-heading">Empty</div><div class="panel-body">This include file has no types, functions, or constants.</div></div>';
        }
        
        echo '</div>';
    }

    if (!$AnythingRendered) {
        echo '<div class="panel panel-danger"><div class="panel-heading">No results</div><div class="panel-body">Nothing was found matching your search query.</div></div>';
    }

    function GetTypeHeader($Type)
    {
        switch ($Type)
        {
            case 'class': return '<div class="panel panel-danger"><div class="panel-heading">Classes</div>';
            case 'enum': return '<div class="panel panel-warning"><div class="panel-heading">Types</div>';
            case 'forward': return '<div class="panel panel-info"><div class="panel-heading">Forwards</div>';
            case 'function':
                return '<div class="panel panel-success"><div class="panel-heading">Functions</div>';
        }
        
        return '<div class="panel panel-primary"><div class="panel-heading">' . $Type . '</div>';
    }
