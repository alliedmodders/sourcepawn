native bool IsClientConnected(int client);
native int GetUserFlagBits(int client);
native int ReadFlagString(const char[] flags, int &numchars=0);

public KickSpec(timer, iClient)
{
	if(!IsClientConnected(iClient) || (GetUserFlagBits(iClient) & ReadFlagString("s")))
		return;
}
