native void printnums(...);

enum struct Numbers
{
	int first;
	int second;
	int third;
}

static Numbers list[] = {
	{ 1, 2 },
	{ 4, 5, 6 },
	{ 7 },
}

public void main()
{
	for (int i = 0; i < sizeof(list); i++) {
		printnums(i, list[i].first, list[i].second, list[i].third);
	}
}
