#include <shell>

int Counter = 0;

int NeedsHeap(int& x = 0) {
	return Counter;
}

public void main()
{
	while (NeedsHeap() < 4000)
		Counter++;

	printnum(Counter);

	Counter = 0;
	do {
		Counter++;
	} while (NeedsHeap() < 4000);

	printnum(Counter);

	for (; NeedsHeap() < 4000; NeedsHeap(), Counter++) 
	{}

	printnum(Counter);
}
