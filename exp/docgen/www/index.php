<?php
// vim: set ts=4 sw=4 tw=99 et:
    require_once __DIR__ . '/../settings.php';
    require_once __DIR__ . '/template/render.php';
    require_once __DIR__ . '/search.php';
    
    $Path = isset( $_SERVER[ 'QUERY_STRING' ] ) ? trim( $_SERVER[ 'QUERY_STRING' ], '/' ) : '';
    $RenderLayout = !isset( $_SERVER[ 'HTTP_X_PJAX' ] ) || $_SERVER[ 'HTTP_X_PJAX' ] !== 'true';
    
    if (substr($Path, 0, 8) === '__search')
        $RenderLayout = false;
    
    if ($RenderLayout) {
        $CurrentOpenFile = false;
        $CurrentOpenFunction = false;
        
        $Query = 'select id, name from spdoc_include order by name asc';
        $Includes = $Database->query($Query)->fetchAll(PDO::FETCH_KEY_PAIR);
        
        // Function list per file.
        $Functions = Array();
        $Query =
            'select i.name as include_name, f.kind, f.name, f.brief ' .
            'from spdoc_function f ' .
            'join spdoc_include i ' .
            ' on f.include_id = i.id ' .
            'where f.parent_id is null ' .
            'order by f.kind, f.name asc';
        $STH = $Database->query($Query);
        while ($Function = $STH->fetch()) {
            $Functions[$Function['include_name']][] = Array(
                'Function' => $Function['name'],
                'Comment' => $Function['brief'],
                'Type' => ($Function['kind'] === 'forward' ? 'forward' : 'function'),
            );
        }

        // Class list per file.
        $Classes = Array();
        $Query =
            'select i.name as include_name, c.name, c.brief ' .
            'from spdoc_class c ' .
            'join spdoc_include i ' .
            ' on c.include_id = i.id ' .
            'order by c.name asc';
        $STH = $Database->query($Query);
        while ($Class = $STH->fetch()) {
            $Classes[$Class['include_name']][] = Array(
                'Name' => $Class['name'],
                'Comment' => $Class['brief'],
            );
        }

        // Enum list per file.
        $Types = Array();
        $Query =
            'select i.name as include_name, e.name, e.brief ' .
            'from spdoc_enum e ' .
            'join spdoc_include i ' .
            ' on e.include_id = i.id ' .
            'order by e.name asc';
        $STH = $Database->query($Query);
        while ($Enum = $STH->fetch()) {
            $Types[$Enum['include_name']][] = Array(
                'Name' => $Enum['name'],
                'Comment' => $Enum['brief'],
            );
        }

        $Query = 
            'select i.name as include_name, t.name, t.brief ' .
            'from spdoc_type t ' .
            'join spdoc_include i ' .
            ' on t.include_id = i.id ' .
            'order by t.name asc';
        $STH = $Database->Query($Query);
        while ($Typedef = $STH->fetch()) {
            $Types[$Typedef['include_name']][] = Array(
                'Name' => $Typedef['name'],
                'Comment' => $Typedef['brief'],
            );
        }

        foreach ($Types as $_ => &$list) {
            usort($list, function ($a, $b) {
                return strcmp($a['Name'], $b['Name']);
            });
        }
    }
    
    if ($Path) {
        $Path = explode('/', $Path, 3);
        
        $Action = !empty($Path[1]) ? filter_var($Path[1], FILTER_SANITIZE_STRING) : false;
        if (!isset($Path[0])) {
            require __DIR__ . '/template/404.php';
            exit;
        }

        $IncludeName = filter_var($Path[0], FILTER_SANITIZE_STRING);
        if ($IncludeName === '__search') {
            if (empty($Action))
                exit;
            
            $Action = '%' . Str_Replace(Array('\\', '%', '_'), Array('\\\\', '\%', '\_'), $Action) . '%';
            
            $Query =
                'select i.name as include_name, c.brief ' .
                'from spdoc_constant c ' .
                'join spdoc_include i ' .
                ' on c.include_id = i.id ' .
                'where name like ? or brief like ?';
            $STH = $Database->prepare($Query);
            $STH->execute(Array($Action, $Action));
            
            $Results = $STH->fetchAll();
            
            echo json_encode( $Results );
            exit;
        }
        
        $HeaderTitle = $CurrentOpenFile = $IncludeName;

        if ($Action) {
            if ($Action === '__raw') {
                $Query = 'select content from spdoc_include where name = :includeName';
                $STH = $Database->prepare($Query);
                $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
                $STH->execute();

                $PageFile = $STH->fetch();

                if (Empty($PageFile)) {
                    require __DIR__ . '/template/404.php';
                    exit;
                }

                require __DIR__ . '/template/raw.php';
                exit;
            }

            if (isset($Path[2]))
                $Object = FindSubObject($Database, $IncludeName, $Action, $Path[2]);
            else
                $Object = FindObject($Database, $IncludeName, $Action);
            if ($Object === null) {
                require __DIR__ . '/template/404.php';
                exit;
            }
            $ObjectType = $Object['type'];
            $Object = $Object['data'];
            if ($ObjectType === 'function' || $ObjectType === 'method') {
                $PageFunction = $Object;
                require __DIR__ . '/template/function.php';
                exit;
            }
            if ($ObjectType === 'class') {
                $PageClass = $Object;
                require __DIR__ . '/template/class.php';
                exit;
            }
            if ($ObjectType === 'property') {
                $PageProperty = $Object;
                require __DIR__ . '/template/property.php';
                exit;
            }
            if ($ObjectType === 'enum') {
                $PageEnum = $Object;
                require __DIR__ . '/template/enum.php';
                exit;
            }
            if ($ObjectType === 'type') {
                $PageType = $Object;
                require __DIR__ . '/template/type.php';
                exit;
            }
        }

        $Query = 'select name from spdoc_include where name = :includeName';
        $STH = $Database->prepare($Query);
        $STH->bindValue(':includeName', $IncludeName, PDO::PARAM_STR);
        $STH->execute();

        $PageFile = $STH->fetch();

        if (Empty($PageFile)) {
            require __DIR__ . '/template/404.php';
            exit;
        }

        $PageFunctions = FindFunctions($Database, $IncludeName);
        $PageClasses = FindClasses($Database, $IncludeName);
        $PageEnums = FindEnums($Database, $IncludeName);
        $PageConstants = FindConstants($Database, $IncludeName);
        $PageCallbacks = FindCallbacks($Database, $IncludeName);
        $PageTypes = FindTypes($Database, $IncludeName);

        require __DIR__ . '/template/include.php';
        exit;
    }

    require __DIR__ . '/template/main.php';
