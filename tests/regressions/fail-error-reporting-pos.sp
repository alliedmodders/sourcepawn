// warnings_are_errors: true
// type: compiler-output

int i;
bool b;
enum e { e1 };

public void main()
{
	if (i > b ) {}
	if (e1 > b ) {}
	
	i > b;
	e1 > b;
}

/*


















*/
