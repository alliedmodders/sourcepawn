int g_bTest[32 + 1] = { 0, ... };

native any GetNativeCell(int index);

public main()
{
    return g_bTest[GetNativeCell(1)];
}
