#define CSS 1

public main() {
#if defined CSS
	new bool:headshot = true;
#elseif defined TF2
	new customkill = GetEventInt(event, "customkill");
	new bool:headshot = (customkill == 1);
#elseif defined DODS
	new bool:headshot = (hurtHitGroup[victimClient] == 1);			
#else
	new bool:headshot = false;
#endif		
	return headshot;
}
