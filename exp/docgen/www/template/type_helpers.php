<?php
function ArrayToSql($Array)
{
    return implode(', ', array_map(function($Item)
    {
        return '"' . preg_replace('/[^A-Za-z0-9_]/', '', $Item) . '"';
    }, $Array));
}

// Poor man's syntax highlighter
function HighlightTypes($Syntax, $Parameters, $Types)
{
    $Syntax = htmlspecialchars($Syntax);
    
    if (empty($Parameters))
    {
        return $Syntax;
    }
    
    global $BaseURL;
    
    $Syntax = preg_replace_callback('/(\(|, )([A-Za-z0-9_]+)(\[\])? /', function($match) use ($BaseURL, $Types)
    {
        if (isset($Types[$match[2]]))
        {
            $Replacement = '<a href="' . $BaseURL . $Types[$match[2]] . '/' . $match[2] . '" class="type">' . $match[2] . '</a>';
        }
        else
        {
            $Replacement = '<span class="type">' . $match[2] . '</span>';
        }
        
        return str_replace($match[2], $Replacement, $match[0]);
    }, $Syntax);
    
    return $Syntax;
}

function NormalizeType($Type)
{
    $Pos = strrpos($Type, '&');
    
    if ($Pos === false)
    {
        $Pos = strpos($Type, ' ...');
        
        if ($Pos === false)
        {
            $Pos = strpos($Type, '[');
        }
    }
    
    if ($Pos !== false)
    {
        return substr($Type, 0, $Pos);
    }
    
    return $Type;
}

function IsDefaultType($Type)
{
    // isset is faster than in_array, so just assign everything to true
    $Types = [
        'float' => true,
        'any' => true,
        'bool' => true,
        'int' => true,
        'char' => true,
        'void' => true,
        'function' => true,
        'null_t' => true,
        'nullfunc_t' => true,
    ];
    
    return isset($Types[$Type]);
}

function LookupTypes($Parameters)
{
    $Types = [];
    $TypesToLookup = [];
    
    foreach ($Parameters as $Param)
    {
        $Type = NormalizeType($Param['type']);
        
        if (!IsDefaultType($Type))
        {
            $TypesToLookup[$Type] = $Type;
        }
    }
    
    if (!empty($TypesToLookup))
    {
        global $Database;
        
        // Enums
        $Query =
            'select i.name as include_name, e.name ' .
            'from spdoc_enum e ' .
            'join spdoc_include i ' .
            ' on e.include_id = i.id ' .
            'where e.name IN (' . ArrayToSql($TypesToLookup) . ')';
        $STH = $Database->query($Query);
        while ($Type = $STH->fetch())
        {
            unset($TypesToLookup[$Type['name']]);
            
            $Types[$Type['name']] = $Type['include_name'];
        }
        
        // Types
        if (!empty($TypesToLookup))
        {
            $Query = 
                'select i.name as include_name, t.name ' .
                'from spdoc_type t ' .
                'join spdoc_include i ' .
                ' on t.include_id = i.id ' .
                'where t.name IN (' . ArrayToSql($TypesToLookup) . ')';
            $STH = $Database->query($Query);
            while ($Type = $STH->fetch())
            {
                unset($TypesToLookup[$Type['name']]);
                
                $Types[$Type['name']] = $Type['include_name'];
            }
        }
        
        // Classes
        if (!empty($TypesToLookup))
        {
            $Query = 
                'select i.name as include_name, c.name ' .
                'from spdoc_class c ' .
                'join spdoc_include i ' .
                ' on c.include_id = i.id ' .
                'where c.name IN (' . ArrayToSql($TypesToLookup) . ')';
            $STH = $Database->query($Query);
            while ($Type = $STH->fetch())
            {
                $Types[$Type['name']] = $Type['include_name'];
            }
        }
    }
    
    return $Types;
}
