#include <handles>

methodmap Panel < Handle {
}
native Handle CreatePanel(Handle hStyle=INVALID_HANDLE);
native bool DrawPanelText(Handle panel, const char[] text);

public main()
{
    new Handle:Panel = CreatePanel(INVALID_HANDLE);
    DrawPanelText(Panel, "hello");
}
