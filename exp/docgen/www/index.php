<?php
// vim: set ts=4 sw=4 tw=99 et:
    require_once __DIR__ . '/../settings.php';
    require_once __DIR__ . '/template/render.php';
    require_once __DIR__ . '/search.php';
    
    $Path = isset( $_SERVER[ 'QUERY_STRING' ] ) ? trim( $_SERVER[ 'QUERY_STRING' ], '/' ) : '';
    $RenderLayout = !isset( $_SERVER[ 'HTTP_X_PJAX' ] ) || $_SERVER[ 'HTTP_X_PJAX' ] !== 'true';

    // Stupid bugfix when clicking on homepage
    if (substr($Path, 0, 6) === '_pjax=') {
        $Path = '';
    }

    if ($RenderLayout) {
        $Search = false;
        
        if (substr($Path, 0, 8) === '__search')
        {
            $Path = substr($Path, 9);

            if (strlen($Path) < 2)
            {
                http_response_code(400);

                exit;
            }

            $Search = '%' . Str_Replace( Array( '\\', '%', '_' ), Array( '\\\\', '\%', '\_' ), $Path ) . '%';
        }

        $CurrentOpenFile = false;
        $CurrentOpenObject = false;

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
            ($Search !== false ? 'and f.name like :search ' : '') .
            'order by f.kind, f.name asc';
        $STH = $Database->prepare($Query);
        if ($Search !== false) {
            $STH->bindValue(':search', $Search, PDO::PARAM_STR);
        }
        $STH->execute();
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
            ($Search !== false ? 'where c.name like :search ' : '') .
            'order by c.name asc';
        $STH = $Database->prepare($Query);
        if ($Search !== false) {
            $STH->bindValue(':search', $Search, PDO::PARAM_STR);
        }
        $STH->execute();
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
            ($Search !== false ? 'where e.name like :search' : '');
        $STH = $Database->prepare($Query);
        if ($Search !== false) {
            $STH->bindValue(':search', $Search, PDO::PARAM_STR);
        }
        $STH->execute();
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
            ($Search !== false ? 'where t.name like :search' : '');
        $STH = $Database->prepare($Query);
        if ($Search !== false) {
            $STH->bindValue(':search', $Search, PDO::PARAM_STR);
        }
        $STH->execute();
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

        if ($Search !== false) {
            $CurrentOpenFile = '%all%';

            require __DIR__ . '/template/sidebar.php';

            exit;
        }
    }
    
    if ($Path) {
        $Path = explode('/', $Path, 3);
        $Action = !empty($Path[1]) ? filter_var($Path[1], FILTER_SANITIZE_STRING) : false;

        if (!isset($Path[0])) {
            require __DIR__ . '/template/404.php';
            exit;
        }

        $HeaderTitle = $CurrentOpenFile = $IncludeName = filter_var($Path[0], FILTER_SANITIZE_STRING);

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

            if (isset($Path[2])) {
                $Object = FindSubObject($Database, $IncludeName, $Action, $Path[2]);
            } else {
                $Object = FindObject($Database, $IncludeName, $Action);
            }

            if ($Object === null) {
                require __DIR__ . '/template/404.php';
                exit;
            }

            $ObjectType = $Object['type'];
            $Object = $Object['data'];

            if ($ObjectType === 'method' || $ObjectType === 'property') {
                $CurrentOpenObject = $Object['class_name'];
            } else {
                $CurrentOpenObject = $Object['name'];
            }

            if ($ObjectType === 'function' || $ObjectType === 'method') {
                $HeaderTitle = ($ObjectType === 'method' ? ($Object['class_name'] . '.') : '')
                    . $Object['name'] . ' · ' . $HeaderTitle;
                $PageFunction = $Object;
                require __DIR__ . '/template/function.php';
                exit;
            }
            if ($ObjectType === 'class') {
                $HeaderTitle = $Object['name'] . ' · ' . $HeaderTitle;
                $PageClass = $Object;
                require __DIR__ . '/template/class.php';
                exit;
            }
            if ($ObjectType === 'property') {
                $HeaderTitle = $Object['class_name'] . '.' . $Object['name'] . ' · ' . $HeaderTitle;
                $PageProperty = $Object;
                require __DIR__ . '/template/property.php';
                exit;
            }
            if ($ObjectType === 'enum') {
                $HeaderTitle = $Object['name'] . ' · ' . $HeaderTitle;
                $PageEnum = $Object;
                require __DIR__ . '/template/enum.php';
                exit;
            }
            if ($ObjectType === 'type') {
                $HeaderTitle = $Object['name'] . ' ' . $Object['kind'] . ' · ' . $HeaderTitle;
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
        $PageForwards = FindForwards($Database, $IncludeName);
        $PageTypes = FindTypes($Database, $IncludeName);

        require __DIR__ . '/template/include.php';
        exit;
    }

    require __DIR__ . '/template/main.php';
