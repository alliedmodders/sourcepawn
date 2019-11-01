// warnings_are_errors: true
#pragma newdecls required
#pragma semicolon 1

native void Call_StartFunction(Function func);
native void Call_PushCell(any value);
native int Call_Finish(any &result=0);

typedef Type = function void (const bool bSuccessful);
stock static Type s_fnCallback;

public void main()
{
	bool bSuccessful;
	if ( s_fnCallback != DoNothing )
	{
		Call_StartFunction( s_fnCallback );
		Call_PushCell( bSuccessful );
		Call_Finish();
		s_fnCallback = DoNothing;
	}
}

stock static void DoNothing (const bool bSuccessful)
{
}
