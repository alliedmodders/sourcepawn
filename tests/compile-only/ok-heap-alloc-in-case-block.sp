#define MAX_ENTITIES 	2048 	// Cantidad Máxima De Entidades
#define TYPE_FLAME 	1 	// Tipo Especial, Llamarada
#define TYPE_LASER 	2 	// Tipo Especial, Láser
#define TYPE_TESLA 	3 	// Tipo Especial, Tesla
#define TYPE_FREEZE 4   // Tipo Especial, Congelante
#define TYPE_NAUSEATING 5 // Tipo Especial, Nauseabundo

native void SetEntProp(int entity, PropType type, const char[] prop, any value, int size=4, int element=0);

enum PropType
{
	Prop_Send = 0,	/**< This property is networked. */
	Prop_Data = 1,	/**< This property is for save game data fields. */
};

public void StartGlowing( int entity, int TeamIndex )
{
	static char sColor[16];
	
	switch( entity )
	{
		case TYPE_NAUSEATING: sColor = GetColorIndex( 10 );
	}
}

stock char[] GetColorIndex( int iColorType )
{
	static char sColor[16];
	return sColor;
}

stock int GetColor( char[] sTemp ) // Convierte una cadena de texto en un valor entero.
{
	int iColor;
	return iColor;
}
