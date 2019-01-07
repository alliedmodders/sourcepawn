#define MAX_EFFECTOR_NAME_LENGTH 50
#define MAX_EFFECTOR_CONCURRENT 10
#define MAX_EFFECTOR_PARAMETER 4
#define MAXPLAYERS 65

#define BASE_EFFECTOR_CYCLE 1.0

Effector g_UserEffectorData[MAXPLAYERS+1][MAX_EFFECTOR_CONCURRENT];

enum struct Interface_Effector
{
	float cycle;
	char name[MAX_EFFECTOR_NAME_LENGTH];

	void Perform(int client, any param1=0, any param2=0, any param3=0, any param4=0){


	}
}

enum struct Effector
{
	int index;
	any data[MAX_EFFECTOR_PARAMETER];
}

public void OnPluginStart(){

	g_UserEffectorData[0][0].data[0] = 0;
}
