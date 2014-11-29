<?php
	require __DIR__ . '/../settings.php';
	
	$Path = isset( $_SERVER[ 'QUERY_STRING' ] ) ? trim( $_SERVER[ 'QUERY_STRING' ], '/' ) : '';
	$RenderLayout = !isset( $_SERVER[ 'HTTP_X_PJAX' ] ) || $_SERVER[ 'HTTP_X_PJAX' ] !== 'true';
	
	if( substr( $Path, 0, 8 ) === '__search' )
	{
		$RenderLayout = false;
	}
	
	if( $RenderLayout )
	{
		$CurrentOpenFile = false;
		$CurrentOpenFunction = false;
		
		$Includes = $Database->query( 'SELECT `ID`, `IncludeName` FROM `' . $Columns[ 'Files' ] . '` ORDER BY `IncludeName` ASC' )->fetchAll( PDO :: FETCH_KEY_PAIR );
		
		$Functions = Array();
		
		$STH = $Database->query( 'SELECT `Function`, `Type`, `Comment`, `IncludeName` FROM `' . $Columns[ 'Functions' ] . '` ORDER BY `Type` ASC, `Function` ASC' );
		
		while( $Function = $STH->fetch() )
		{
			$Functions[ $Function[ 'IncludeName' ] ][ ] = Array(
				'Function' => $Function[ 'Function' ],
				'Comment' => $Function[ 'Comment' ],
				'Type' => $Function[ 'Type' ],
			);
		}
	}
	
	if( $Path )
	{
		$Path = explode( '/', $Path, 2 );
		
		$Action = !empty( $Path[ 1 ] ) ? filter_var( $Path[ 1 ], FILTER_SANITIZE_STRING ) : false;
		
		if( isset( $Path[ 0 ] ) )
		{
			$IncludeName = filter_var( $Path[ 0 ], FILTER_SANITIZE_STRING );
			
			if( $IncludeName === '__search' )
			{
				if( empty( $Action ) )
				{
					exit;
				}
				
				$Action = '%' . Str_Replace( Array( '\\', '%', '_' ), Array( '\\\\', '\%', '\_' ), $Action ) . '%';
				
				$STH = $Database->prepare( 'SELECT `IncludeName` as `includeName`, `Comment` as `value` FROM `' . $Columns[ 'Constants' ] . '` WHERE `Constant` LIKE ? OR `Comment` LIKE ?' );
				$STH->execute( Array( $Action, $Action ) );
				
				$Results = $STH->fetchAll();
				
				echo json_encode( $Results );
				
				exit;
			}
			
			$HeaderTitle = $CurrentOpenFile = $IncludeName;
			
			if( $Action )
			{
				if( $Action === '__raw' )
				{
					KidsNeverUseGotosPlease_ShowRaw:
					
					$STH = $Database->prepare( 'SELECT `Content` FROM `' . $Columns[ 'Files' ] . '` WHERE `IncludeName` = :includeName' );
					$STH->bindValue( ':includeName', $IncludeName, PDO :: PARAM_STR );
					$STH->execute();
					
					$PageFile = $STH->fetch();
					
					if( Empty( $PageFile ) )
					{
						require __DIR__ . '/template/404.php';
						
						exit;
					}
					
					require __DIR__ . '/template/raw.php';
				}
				else if( $Action === '__functions' )
				{
					$NotGoto = true;
					
					KidsNeverUseGotosPlease_ShowFunctions:
					
					$STH = $Database->prepare( 'SELECT `Function`, `Comment` FROM `' . $Columns[ 'Functions' ] . '` WHERE `IncludeName` = :includeName' );
					$STH->bindValue( ':includeName', $IncludeName, PDO :: PARAM_STR );
					$STH->execute();
					
					$PageFunctions = $STH->fetchAll();
					
					if( Empty( $PageFunctions ) )
					{
						if( isset( $NotGoto ) )
						{
							header( 'Location: ' . $BaseURL . $IncludeName );
							exit;
						}
						else
						{
							goto KidsNeverUseGotosPlease_ShowRaw; // I use goto to prevent unnecessary redirects, and to match behaviour of old SourceMod's docgen
						}
					}
					
					$HeaderTitle = 'Functions · ' . $HeaderTitle;
					
					require __DIR__ . '/template/functions.php';
				}
				else
				{
					$STH = $Database->prepare( 'SELECT `Function`, `FullFunction`, `Type`, `Comment`, `Tags`, `IncludeName` FROM `' . $Columns[ 'Functions' ] . '` WHERE `Function` = :functionName AND `IncludeName` = :includeName' );
					$STH->bindValue( ':includeName', $IncludeName, PDO :: PARAM_STR );
					$STH->bindValue( ':functionName', $Action, PDO :: PARAM_STR );
					$STH->execute();
					
					$PageFunction = $STH->fetch();
					
					if( Empty( $PageFunction ) )
					{
						require __DIR__ . '/template/404.php';
						
						exit;
					}
					
					$CurrentOpenFunction = $PageFunction[ 'Function' ];
					
					$HeaderTitle = $PageFunction[ 'Function' ] . ' · ' . $HeaderTitle;
					
					require __DIR__ . '/template/function.php';
				}
			}
			else
			{
				$STH = $Database->prepare( 'SELECT `Constant`, `Comment`, `Tags` FROM `' . $Columns[ 'Constants' ] . '` WHERE `IncludeName` = :includeName' );
				$STH->bindValue( ':includeName', $IncludeName, PDO :: PARAM_STR );
				$STH->execute();
				
				$Results = $STH->fetchAll();
				
				if( Empty( $Results ) )
				{
					goto KidsNeverUseGotosPlease_ShowFunctions; // I use goto to prevent unnecessary redirects, and to match behaviour of old SourceMod's docgen
				}
				
				$PageName = $IncludeName;
				
				$HeaderTitle = 'Constants · ' . $HeaderTitle;
				
				require __DIR__ . '/template/constants.php';
			}
		}
		else
		{
			require __DIR__ . '/template/404.php';
		}
		
		exit;
	}
	
	require __DIR__ . '/template/main.php';
