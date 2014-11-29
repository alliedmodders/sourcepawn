<?php
	require __DIR__ . '/header.php';
?>

<ol class="breadcrumb">
	<li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
	<li class="active">Constants</li>
	
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__functions">Functions</a></li>
</ol>

<h1 class="page-header">List of constants in <?php echo htmlspecialchars( $PageName ); ?>.inc</h1>

<?php
	$InSection = 0;
	$InSectionBody = 0;
	
	foreach( $Results as $Result )
	{
		$ClosePanel = false;
		$Tags = json_decode( $Result[ 'Tags' ], true );
		
		if( substr( $Result[ 'Comment' ], 0, 8 ) === '@section' )
		{
			$InSection++;
			
			$Slug = StringToSlug( $Result[ 'Comment' ] );
			
			echo '<div class="panel panel-info" id="' . $Slug . '">';
			echo '<div class="panel-heading">' . htmlspecialchars( substr( $Result[ 'Comment' ], 9 ) ) . '<a href="#' . $Slug . '" class="permalink pull-right">#</a></div>';
			
			if( Empty( $Tags ) && Empty( $Result[ 'Constant' ] ) )
			{
				$InSectionBody++;
				
				echo '<div class="panel-body">';
				
				continue;
			}
		}
		else if( $InSection > 0 && $Result[ 'Comment' ] === '@endsection' )
		{
			$InSection--;
			
			if( $InSectionBody > 0 )
			{
				$InSectionBody--;
				
				echo '</div>';
			}
			
			echo '</div>';
			
			continue;
		}
		else
		{
			$Slug = StringToSlug( $Result[ 'Comment' ] );
			
			echo '<div class="panel panel-primary" id="' . $Slug . '">';
			echo '<div class="panel-heading">' . htmlspecialchars( $Result[ 'Comment' ] ) . '<a href="#' . $Slug . '" class="permalink pull-right">#</a></div>';
			
			$ClosePanel = true;
		}
		
		if( !Empty( $Tags ) )
		{
			echo '<div class="panel-body">';
			
			foreach( $Tags as $Tag )
			{
				echo '<h4 class="sub-header2">' . ucfirst( $Tag[ 'Tag' ] ) . '</h4>';
				
				if( isset( $Tag[ 'Description' ] ) )
				{
					echo '<pre class="description">' . htmlspecialchars( $Tag[ 'Description' ] ) . '</pre>';
				}
			}
			
			echo '</div>';
		}
		
		if( !Empty( $Result[ 'Constant' ] ) )
		{
			echo '<div class="panel-footer"><pre class="description">' . htmlspecialchars( $Result[ 'Constant' ] ) . '</pre></div>';
		}
		
		if( $ClosePanel )
		{
			echo '</div>';
		}
	}
	
	while( --$InSection > 0 )
	{
		echo '</div>';
	}
	
	while( --$InSectionBody > 0 )
	{
		echo '</div>';
	}
?>

<?php
	require __DIR__ . '/footer.php';
	
	function StringToSlug( $String )
	{
		$String = preg_replace( '/[^A-Za-z0-9-]+/', '-', $String );
		$String = trim( $String, "- \t\n\r\0\x0B" );
		
		return strtolower( $String );
	}
?>
