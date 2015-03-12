(function()
{
	var baseUrl = $( 'body' ).data( 'baseurl' ),
		fullBaseUrl = new RegExp( '^' + escapeRegExp( location.origin + baseUrl ) );
	
	$( '.function' ).popover(
	{
		container: 'body',
		placement: 'right',
		trigger: 'hover'
	} );
	
	$( document )
		.pjax( 'a', '#pjax-container', { timeout: 8000 } )
		.on( 'pjax:start', function() { NProgress.start(); } )
		.on( 'pjax:end',   function() { NProgress.done();  } )
		.on( 'pjax:clicked pjax:popstate', function( ev )
			{
				$( '.function.active' ).removeClass( 'active' );
				
				var parent = $( ev.target ).parent();
				
				if( parent.hasClass( 'function' ) )
				{
					parent.addClass( 'active' );
				}
				// Try to figure out which function user clicked on
				// and mark that function as active in the sidebar
				else
				{
					if( $( ev.target ).hasClass( 'file' ) )
					{
						return;
					}
					
					var relativeUrl = ( ev.type === 'pjax:popstate' ? ev.state.url : ev.target.href ).replace( fullBaseUrl, '' ),
						pos = relativeUrl.indexOf( '/' ),
						file = relativeUrl;
					
					if( pos > 0 )
					{
						file = file.substring(0, pos);
						
						relativeUrl = relativeUrl.substring( pos + 1 );
						
						pos = relativeUrl.indexOf( '/' );
						
						if( pos > 0 )
						{
							relativeUrl = relativeUrl.substring( 0, pos );
						}
						
						$( 'li[data-title="' + relativeUrl + '"]' ).first().addClass( 'active' );
					}
					
					file = $( '#file-' + file );
					
					if( file )
					{
						var visibleNav = $( '.nav-functions.show' );
						
						if( !visibleNav.is( file ) )
						{
							visibleNav.removeClass( 'show' );
						}
						
						file.addClass( 'show' );
					}
				}
			} );
	
	$( '.file' ).on( 'click', function()
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
	
	var value,
		lastValue,
		elementSearch = $( '.typeahead' ),
		elementFunctions = $( '#js-functions' ),
		elementFunctionsDefault = $( '#js-functions-default' ),
		elementClear = $( '#js-search-clear' );
	
	elementClear.on( 'click', function()
	{
		lastValue = value = '';
		
		elementFunctions.hide().empty();
		elementFunctionsDefault.show();
		elementSearch.val( '' );
		elementClear.hide();
	} );
	
	elementSearch.on( 'change', function()
	{
		value = elementSearch.val();
		
		if( value === lastValue )
		{
			return;
		}
		
		lastValue = value;
		
		if( value.length > 0 )
		{
			elementClear.show();
		}
		else
		{
			elementClear.hide();
		}
		
		if( value.length < 2 )
		{
			elementFunctions.hide().empty();
			elementFunctionsDefault.show();
			
			return;
		}
		
		elementSearch.attr( 'disabled', true );
		
		NProgress.start();
		
		elementFunctions.load( baseUrl + '__search/' + value, function()
		{
			elementFunctionsDefault.hide();
			elementFunctions.show();
			
			elementFunctions.find( '.function' ).popover(
			{
				container: 'body',
				placement: 'right',
				trigger: 'hover'
			} );
			
			elementSearch.removeAttr( 'disabled' );
			
			NProgress.done();
		} );
	} );
	
	function escapeRegExp( str )
	{
		return str.replace( /[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&' );
	}
}());
