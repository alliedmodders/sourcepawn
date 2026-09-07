#include <shell>

int strcopy(char[] dest, int dest_len, const char[] src) {
    if (dest_len == 0)
        return 0;

    int i = 0;
    for (; i < dest_len - 1; i++) {
        if (!src[i])
            break;
        dest[i] = src[i];
    }
    dest[i] = 0;
    return i - 1;
}

public main()
{
    for (int i = 0; i < 2; i++)
    {
        char text[16] = "before";
        printstrs("text=", text, "\n");
        strcopy(text, sizeof(text), "changed");

        char exact[7] = "before";
        printstrs("exact=", exact, "\n");
        exact[0] = 'X';

        int parts[6] = {1, 2, 3};
        printnum(parts[3] + parts[4] + parts[5]);
        parts[3] = 9;
        parts[4] = 9;
        parts[5] = 9;
    }
}
