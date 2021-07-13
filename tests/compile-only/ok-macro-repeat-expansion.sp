#define FOO 5
#define BAR 7

#define DUCK FOO + BAR
#define QUACK FOO + FOO

public int Apple()
{
	return DUCK;	//no error
}

public int Bread()
{
	return QUACK;	//error 017: undefined symbol "FOO"
}
