// warnings_are_errors: true
forward void OnClientPostAdminCheck(int x);

public void OnClientPostAdminCheck(int x) {}

public Function OnPluginStart()
{
  return OnClientPostAdminCheck;
}
