#include <handles>

#define MAXPLAYERS 64

Handle g_PlayerLists[MAXPLAYERS + 1];

methodmap TestPlayer
{
	public TestPlayer(int client)
	{
		return view_as<TestPlayer>(client);
	}
	
	property Handle m_list
	{
		public get()
		{
			return g_PlayerLists[view_as<int>(this)];
		}
	}
}

public void OnClientPutInServer(int client)
{
	// Removing this line allows the compile to go through
	delete TestPlayer(client).m_list;
}
