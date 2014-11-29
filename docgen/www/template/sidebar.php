<?php
	foreach( $Includes as $File )
	{
		echo '<h4 class="file"><a data-file="' . $File . '" href="' . $BaseURL . $File . '">' . $File . '</a></h4>';
		echo '<div class="nav-functions ' . ( $CurrentOpenFile === $File ? ' show' : '' ) . '" id="file-' . $File . '">';
		
		if( !empty( $Functions[ $File ] ) )
		{
			$PreviousFunctionType = 'hypehypehype';
			$OpenList = false;
			
			foreach( $Functions[ $File ] as $Function )
			{
				if( $PreviousFunctionType !== $Function[ 'Type' ] )
				{
					$PreviousFunctionType = $Function[ 'Type' ];
					
					if( $OpenList )
					{
						echo '</ul></div></div>';
					}
					
					$OpenList = true;
					
					echo GetFunctionHeader( $Function[ 'Type' ] ) . '<div class="panel-body panel-sidebar"><ul class="nav nav-sidebar">';
				}
				
				$FunctionName = htmlspecialchars( $Function[ 'Function' ] );
				
				echo '<li class="function' . ( $CurrentOpenFunction === $FunctionName ? ' active' : '' ) . '" data-title="' . $FunctionName. '" data-content="' . htmlspecialchars( $Function[ 'Comment' ] ) . '">';
				echo '<a href="' . $BaseURL . $File . '/' . urlencode( $Function[ 'Function' ] ) . '">' . $FunctionName . '</a>';
				echo '</li>';
			}
			
			if( $OpenList )
			{
				echo '</ul></div></div>';
			}
		}
		else
		{
			echo '<div class="panel panel-primary"><div class="panel-heading">No functions</div><div class="panel-body">This include file has no functions.</div></div>';
		}
		
		echo '</div>';
	}
	
	function GetFunctionHeader( $Type )
	{
		switch( $Type )
		{
			case 'forward': return '<div class="panel panel-info"><div class="panel-heading">Forwards</div>';
			case 'native': return '<div class="panel panel-success"><div class="panel-heading">Natives</div>';
			case 'stock': return '<div class="panel panel-warning"><div class="panel-heading">Stocks</div>';
			case 'functag': return '<div class="panel panel-danger"><div class="panel-heading">Functags</div>';
		}
		
		return '<div class="panel panel-primary"><div class="panel-heading">' . $Type . '</div>';
	}
