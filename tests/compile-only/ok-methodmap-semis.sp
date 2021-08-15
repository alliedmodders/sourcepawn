#pragma semicolon 1

native IsValidEntity(entity);
native int CreateEntityByName(const char[] className);

methodmap Entity
{
	public Entity(const char[] className) {
		return Entity:CreateEntityByName(className);
	}

	property bool IsValid {
		public get() {
			return IsValidEntity(_:this);
		}
	}
};

public main()
{
}
