<?php
// vim: set ts=4 sw=4 tw=99 et:

function FindFunctions($Database, $IncludeName)
{
    $Query =
        'select f.name, f.brief ' .
        'from spdoc_function f ' .
        'join spdoc_include i ' .
        ' on f.include_id = i.id ' .
        'where i.name = :includeName and ' .
        ' f.parent_id is null and ' .
        ' f.kind <> "forward"';

    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    
    $PageFunctions = $STH->fetchAll();
    if (empty($PageFunctions))
        return null;
    return $PageFunctions;
}

function FindForwards($Database, $IncludeName)
{
    $Query =
        'select f.name, f.brief ' .
        'from spdoc_function f ' .
        'join spdoc_include i ' .
        ' on f.include_id = i.id ' .
        'where i.name = :includeName and ' .
        ' f.parent_id is null and ' .
        ' f.kind = "forward"';

    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    
    $PageFunctions = $STH->fetchAll();
    if (empty($PageFunctions))
        return null;
    return $PageFunctions;
}

function FindClasses($Database, $IncludeName)
{
    $Query =
        'select c.name, c.brief ' .
        'from spdoc_class c ' .
        'join spdoc_include i ' .
        ' on c.include_id = i.id ' .
        'where i.name = :includeName';

    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    
    $Classes = $STH->fetchAll();
    if (empty($Classes))
        return null;
    return $Classes;
}

function FindEnums($Database, $IncludeName)
{
    $Query =
        'select e.name, e.brief, e.data '.
        'from spdoc_enum e ' .
        'join spdoc_include i ' .
        ' on e.include_id = i.id ' .
        'where i.name = :includeName';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    $Results = $STH->fetchAll();
    if (empty($Results))
        return null;
    return $Results;
}

function FindTypes($Database, $IncludeName)
{
    $Query =
        'select t.name, t.kind, t.brief, t.data '.
        'from spdoc_type t ' .
        'join spdoc_include i ' .
        ' on t.include_id = i.id ' .
        'where i.name = :includeName ' .
        ' and t.parent_id is null';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    $Results = $STH->fetchAll();
    if (empty($Results))
        return null;
    return $Results;
}

function FindConstants($Database, $IncludeName)
{
    $Query =
        'select cn.name, cn.brief, cn.data '.
        'from spdoc_constant cn ' .
        'join spdoc_include i ' .
        ' on cn.include_id = i.id ' .
        'where i.name = :includeName and ' .
        ' cn.parent_id is null';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->execute();
    $Results = $STH->fetchAll();
    if (empty($Results))
        return null;
    return $Results;
}

function FindClass($Database, $IncludeName, $Name)
{
    $Query =
        'select c.id, c.name, c.brief, c.data ' .
        'from spdoc_class c ' .
        'join spdoc_include i ' .
        ' on c.include_id = i.id ' .
        'where c.name = :name ' .
        ' and i.name = :includeName';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':name', $Name, PDO::PARAM_STR);
    $STH->execute();

    $Class = $STH->fetch();
    if (empty($Class))
        return null;

    $Query = 
        'select p.name, p.type, p.brief, p.data, ' .
        '       p.getter, p.setter ' .
        'from spdoc_property p ' .
        'join spdoc_include i ' .
        ' on p.include_id = i.id ' .
        'join spdoc_class c ' .
        ' on p.class_id = c.id ' .
        'where p.class_id = :classId';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':classId', $Class['id']);
    $STH->execute();
    $Class['properties'] = $STH->fetchAll();

    $Query = 
        'select f.name, f.brief, f.data ' .
        'from spdoc_function f ' .
        'join spdoc_include i ' .
        ' on f.include_id = i.id ' .
        'join spdoc_class c ' .
        ' on f.parent_id = c.id ' .
        'where f.parent_id = :classId ' .
        ' and f.parent_type = "class"';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':classId', $Class['id']);
    $STH->execute();
    $Class['methods'] = $STH->fetchAll();

    return $Class;
}

function FindFunction($Database, $IncludeName, $Name)
{
    $Query =
        'select f.kind, f.name, f.signature, f.brief, f.data ' .
        'from spdoc_function f ' .
        'join spdoc_include i ' .
        ' on f.include_id = i.id ' .
        'where f.name = :functionName ' .
        ' and i.name = :includeName ' .
        ' and f.parent_id is null';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':functionName', $Name, PDO::PARAM_STR);
    $STH->execute();

    return $STH->fetch();
}

function FindType($Database, $IncludeName, $Name)
{
    $Query =
        'select t.kind, t.name, t.brief, t.data ' .
        'from spdoc_type t ' .
        'join spdoc_include i ' .
        ' on t.include_id = i.id ' .
        'where t.name = :typeName ' .
        ' and i.name = :includeName ' .
        ' and t.parent_id is null';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':typeName', $Name, PDO::PARAM_STR);
    $STH->execute();

    return $STH->fetch();
}

function FindEnum($Database, $IncludeName, $Name)
{
    $Query =
        'select e.id, e.name, e.brief, e.data ' .
        'from spdoc_enum e ' .
        'join spdoc_include i ' .
        '  on e.include_id = i.id ' .
        'where e.name = :name' .
        '  and i.name = :includeName';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':name', $Name, PDO::PARAM_STR);
    $STH->execute();

    $Enum = $STH->fetch();
    if (empty($Enum))
        return null;

    $Query =
        'select cn.name, cn.brief, cn.data ' .
        'from spdoc_constant cn ' .
        'join spdoc_enum e ' .
        '  on e.id = cn.parent_id ' .
        'where cn.parent_type = "enum" ' .
        '  and cn.parent_id = :enumId';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':enumId', $Enum['id']);
    $STH->execute();

    $Enum['entries'] = $STH->fetchAll();
    return $Enum;
}

function FindSubObject($Database, $IncludeName, $ClassName, $SubName)
{
    $Query = 
        'select c.name as class_name, f.name, f.signature, f.brief, f.data ' .
        'from spdoc_function f ' .
        'join spdoc_include i ' .
        '  on f.include_id = i.id ' .
        'join spdoc_class c ' .
        '  on f.parent_id = c.id ' .
        'where c.name = :className ' .
        '  and i.name = :includeName ' .
        '  and f.parent_type = "class" ' .
        '  and f.name = :subName';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':className', $ClassName, PDO::PARAM_STR);
    $STH->bindValue(':subName', $SubName, PDO::PARAM_STR);
    $STH->execute();

    $Object = $STH->fetch();
    if (!empty($Object))
        return Array('type' => 'method', 'data' => $Object);

    $Query = 
        'select c.name as class_name, p.name, p.type, p.brief, p.data, ' .
        '       p.getter, p.setter ' .
        'from spdoc_property p ' .
        'join spdoc_include i ' .
        '  on p.include_id = i.id ' .
        'join spdoc_class c ' .
        '  on p.class_id = c.id ' .
        'where c.name = :className ' .
        '  and i.name = :includeName ' .
        '  and p.name = :subName';
    $STH = $Database->prepare($Query);
    $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
    $STH->bindValue(':className', $ClassName, PDO::PARAM_STR);
    $STH->bindValue(':subName', $SubName, PDO::PARAM_STR);
    $STH->execute();

    $Object = $STH->fetch();
    if (!empty($Object))
        return Array('type' => 'property', 'data' => $Object);

    return null;
}

function FindObject($Database, $IncludeName, $Name)
{
    if (($Object = FindClass($Database, $IncludeName, $Name)))
        return Array('type' => 'class', 'data' => $Object);
    if (($Object = FindFunction($Database, $IncludeName, $Name)))
        return Array('type' => 'function', 'data' => $Object);
    if (($Object = FindEnum($Database, $IncludeName, $Name)))
        return Array('type' => 'enum', 'data' => $Object);
    if (($Object = FindType($Database, $IncludeName, $Name)))
        return Array('type' => 'type', 'data' => $Object);
    return NULL;
}
