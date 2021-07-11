enum struct enumWeaponClass
{
        char WEAPONCLASS[32];
        int WEAPONLEVEL;
}

enumWeaponClass gWeaponClass[][] = { {"M4A1 Anti Zombie", 4} };

public void main() { gWeaponClass[0].WEAPONLEVEL = 0; }
