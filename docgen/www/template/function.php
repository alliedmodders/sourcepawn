<?php
	require __DIR__ . '/header.php';
	
	$Tags = json_decode( $PageFunction[ 'Tags' ], true );
	
	$Parameters = Array();
	$OtherTags = Array();
	
	foreach( $Tags as $Tag )
	{
		if( $Tag[ 'Tag' ] === 'param' )
		{
			$Parameters[ ] = $Tag;
		}
		else
		{
			$OtherTags[ ] = $Tag;
		}
	}
?>

<ol class="breadcrumb">
	<li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>"><?php echo $CurrentOpenFile; ?>.inc</a></li>
	<li><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__functions">Functions</a></li>
	<li class="active"><?php echo htmlspecialchars( $PageFunction[ 'Function' ] ); ?></li>
	
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>/__raw">File</a></li>
	<li class="pull-right"><a href="<?php echo $BaseURL . $CurrentOpenFile; ?>">Constants</a></li>
</ol>

<h1 class="page-header"><?php echo htmlspecialchars( $PageFunction[ 'Function' ] ); ?></h1>

<h4 class="sub-header2">Syntax</h4>
<pre class="syntax"><?php echo htmlspecialchars( $PageFunction[ 'FullFunction' ] ); ?></pre>

<?php if( !empty( $Parameters ) ): ?>
<h4 class="sub-header2">Usage</h4>
<div class="table-responsive">
	<table class="table table-condensed table-bordered">
		<?php
			foreach( $Parameters as $Tag )
			{
				echo '<tr><td>' . htmlspecialchars( $Tag[ 'Variable' ] ) . '</td><td><pre class="description">' . htmlspecialchars( $Tag[ 'Description' ] ) . '</pre></td></tr>';
			}
		?>
	</table>
</div>
<?php endif; ?>

<h4 class="sub-header2">Description</h4>
<pre class="description"><?php echo htmlspecialchars( $PageFunction[ 'Comment' ] ); ?></pre>

<?php if( !empty( $OtherTags ) ): ?>
<?php
	foreach( $OtherTags as $Tag )
	{
		switch( $Tag[ 'Tag' ] )
		{
			case 'noreturn':
			{
				echo '<h4 class="sub-header2">Return</h4>';
				echo '<pre class="description">' . ( $PageFunction[ 'Type' ] === 'forward' ? 'This forward ignores the returned value.' : 'This function has no return value.' ) . '</pre>';
				break;
			}
			case 'deprecated':
			{
				echo '<div class="alert alert-danger" role="alert" style="margin-top:20px">';
				echo '<p>This function has been deprecated, do NOT use it</p>';
				
				if( !empty( $Tag[ 'Description' ] ) )
				{
					echo '<p><strong>Reason:</strong> ' . htmlspecialchars( $Tag[ 'Description' ] ) . '</p>';
				}
				
				echo '</div>';
				break;
			}
			default:
			{
				echo '<h4 class="sub-header2">' . ucfirst( $Tag[ 'Tag' ] ) . '</h4>';
				echo '<pre class="description">' . htmlspecialchars( $Tag[ 'Description' ] ) . '</pre>';
			}
		}
	}
?>
<?php endif; ?>

<?php
	require __DIR__ . '/footer.php';
?>
