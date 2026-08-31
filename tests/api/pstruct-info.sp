#include <shell>

struct Plugin
{
    public const char[] name;
    public const char[] description;
    public const char[] author;
    public const char[] version;
    public const char[] url;
    public int games;
    public bool autoload;
};

public Plugin myinfo =
{
    name        = "API Test",
    description = "Dumps pstruct fields",
    author      = "AlliedModders LLC",
    version     = "1.2.3",
    url         = "https://www.alliedmodders.org/",
    games       = 7,
    autoload    = 1,
};

public main() {
    dump_pstruct("myinfo");
}
