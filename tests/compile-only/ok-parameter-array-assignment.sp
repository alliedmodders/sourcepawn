char g_sOverrideScript[32][32];

stock char[] GetOverrideScriptName()
{
  return g_sOverrideScript[0];
}

public void OnVScriptExecuted(char sOverride[256]) {
  sOverride = GetOverrideScriptName();
}

public void main() {}
