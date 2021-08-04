enum Handle { INVALID_HANDLE };

typeset Timer
{
	/**
 	 * Called when the timer interval has elapsed.
 	 * 
 	 * @param timer			Handle to the timer object.
 	 * @param hndl			Handle passed to CreateTimer() when timer was created.
 	 * @return				Plugin_Stop to stop a repeating timer, any other value for
 	 *						default behavior.
 	 */
	function int(Handle timer, Handle hndl);
	
	/**
 	 * Called when the timer interval has elapsed.
 	 * 
 	 * @param timer			Handle to the timer object.
 	 * @param data			Data passed to CreateTimer() when timer was created.
 	 * @return				Plugin_Stop to stop a repeating timer, any other value for
 	 *						default behavior.
 	 */
	function int(Handle timer, any data);
	
	/**
 	 * Called when the timer interval has elapsed.
 	 * 
 	 * @param timer			Handle to the timer object.
 	 * @return				Plugin_Stop to stop a repeating timer, any other value for
 	 *						default behavior.
 	 */
	function int(Handle timer);
};

native int GetEventInt(Handle event, const char[] key, int defValue=0);
native int GetClientOfUserId(int userid);
native bool GetClientAuthString(int client, char[] auth, int maxlen, bool validate=true);
native int GetConVarInt(Handle convar);
native bool IsFakeClient(int client);
native Handle CreateDataPack();
native void WritePackCell(Handle pack, any cell);
native void WritePackString(Handle pack, const char[] str);
native Handle CreateTimer(float interval, Timer func, any data=INVALID_HANDLE, int flags=0);
native float GetConVarFloat(Handle convar);
native bool IsClientInGame(int client);
native int GetTime(int bigStamp[2]={0,0});

#define MR_VERSION	    "0.10"
#define MAXPLAYERS 64

#define MAXLEN_MAP	    32

#define CVAR_DB_CONFIG	    0
#define CVAR_VERSION	    1
#define CVAR_AUTORATE_TIME  2
#define CVAR_ALLOW_REVOTE   3
#define CVAR_TABLE	    4
#define CVAR_AUTORATE_DELAY 5
#define CVAR_DISMISS	    6
#define CVAR_RESULTS	    7
#define CVAR_NUM_CVARS	    8

#define FLAG_RESET_RATINGS  ADMFLAG_VOTE

new Handle:db = INVALID_HANDLE;
new Handle:g_cvars[CVAR_NUM_CVARS];
new bool:g_SQLite = false;
new Handle:g_admin_menu = INVALID_HANDLE;
new g_lastRateTime[MAXPLAYERS];
new bool:g_dismiss = false;

enum MapRatingOrigin {
    MRO_PlayerInitiated,
    MRO_ViewRatingsByRating,
    MRO_ViewRatingsByMap
};

public Event_PlayerDeath(Handle event, const String:name[], bool:dontBroadcast) {
    new client = 0;
    new autorateTime = 0;

    if (IsClientInGame(client) && g_lastRateTime[client - 1] + autorateTime < GetTime()) {
    }
    return 0;
}
