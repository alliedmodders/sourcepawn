#include <shell>

#define MAXPLAYERS 64
 
public main()
{
    int client = 0;
    int client2 = 6;
    int someInt[MAXPLAYERS + 1] = {123, ...};
    printnums(client, client2, ++someInt[client]);
    printnums(client, client2, someInt[client]++);
    printnums(client, client2, someInt[client]--);
    printnums(client, client2, --someInt[client]);
}
