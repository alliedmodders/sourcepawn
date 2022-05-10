#include <shell>

public void main()
{
        for (int i = 0; i < 100; i++)
        {
                printnum(i);
                Funciton1();
        }
}

void Funciton1(int &ret = 0)
{
        //Create any dynamic array. Bigger array size produces less Funciton1 loops before heap error, and vice versa for more loops
        int[] output = new int[2048];

        //Need a bracket here, for loop can be used instead
        {
                if (Function2())
                        return; //This return is needed to create error, even if this seems unneeded
        }
}

bool Function2(int &ret = 0)    //Ref without being passed by variable
{
        return true;
}
