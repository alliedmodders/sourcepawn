(function()
{
	$( '.function' ).popover(
	{
		container: 'body',
		placement: 'right',
		trigger: 'hover'
	} );
	
	$( document )
		.pjax( 'a', '#pjax-container' )
		.on( 'pjax:start', function() { NProgress.start(); } )
		.on( 'pjax:end',   function() { NProgress.done();  } )
		.on( 'pjax:clicked', function( ev )
			{
				$( '.function.active' ).removeClass( 'active' );
				$( ev.target ).parent().addClass( 'active' );
			} );
	
	$( '.file > a' ).on( 'click', function()
	{
		var nav = $( '#file-' + $( this ).text() );
		
		var visibleNav = $( '.nav-functions.show' );
		
		if( !visibleNav.is( nav ) )
		{
			visibleNav.removeClass( 'show' );
		}
		
		if( nav.hasClass( 'show' ) )
		{
			nav.removeClass( 'show' );
		}
		else
		{
			nav.addClass( 'show' );
		}
	} );
	
	var functions = [];
	
	$( '.function' ).each( function( )
	{
		var $this = $( this );
		
		functions.push( [ $this.data( 'title' ), $this.data( 'content' ) ] );
	} );
	
	var constantSearch = new Bloodhound( {
		datumTokenizer: Bloodhound.tokenizers.obj.whitespace( 'value' ),
		queryTokenizer: Bloodhound.tokenizers.whitespace,
		remote: $( 'body' ).data( 'baseurl' ) + '__search/%QUERY'
	} );
	
	constantSearch.initialize();
	
	$( '.typeahead' ).typeahead(
	{
		hint: true,
		highlight: true,
		minLength: 1
	},
	{
		name: 'functions',
		displayKey: 'value',
		templates:
		{
			header: '<h3 class="tt-name">Functions</h3>'
		},
		source: function( query, callback )
		{
			var matches = [], substrRegex = new RegExp( query, 'i' );
			
			$.each( functions, function( i, str )
			{
				if( substrRegex.test( str ) )
				{
					matches.push( { value: str[ 0 ] } );
				}
			} );
			
			callback( matches );
		}
	},
	{
		name: 'constants',
		displayKey: 'value',
		templates:
		{
			header: '<h3 class="tt-name">Constants</h3>'
		},
		source: constantSearch.ttAdapter()
	} ).on( 'typeahead:selected', function( a, b, source )
	{
		if( source === 'constants' )
		{
			$( 'a[data-file="' + b.includeName + '"]' ).click();
			
			return;
		}
		
		var func = $( '[data-title="' + b.value + '"]' );
		
		$( '.nav-sidebar.show' ).removeClass( 'show' );
		
		func
			.parent().addClass( 'show' )
			.end()
			.find( 'a' ).click();
	} );
}());
