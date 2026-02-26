native Float:FloatMul(Float:oper1, Float:oper2);

native Float:GetRandomFloat();

enum _:Rocketeer {
  bActivated,
  iRockets
};

public OnPluginStart()
{
  decl Float:fAngles[3];

  fAngles[0] = GetRandomFloat() * 360;
  fAngles[1] = GetRandomFloat() * 360;
  fAngles[2] = GetRandomFloat() * 360;
}
