enum Action: {}
typedef NewFuncTag = function Action ( char someString[128] );

native UseNewFuncTag( NewFuncTag func );

public OnPluginStart()
{
	// error 100: function prototypes do not match
	UseNewFuncTag( MyOldFunc );
	// error 100: function prototypes do not match
	UseNewFuncTag( MyNewFunc );
}

public Action:MyOldFunc( String:someString[128] )
{
}
public Action MyNewFunc( char someString[128] )
{
}
