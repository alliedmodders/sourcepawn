typedef CallbackCell = function void (any value);
typedef CallbackRef = function void (any &value);

public void OnPluginStart()
{
	// 4 tests below does report error in 1.11
	
	CallbackCell badcellany = RefAny;	// no error
	ReturnFunction(badcellany);

	CallbackCell badcellint = RefInt;	// no error
	ReturnFunction(badcellint);
	
	CallbackRef badrefany = CellAny;	// no error
	ReturnFunction(badrefany);
	
	CallbackRef badrefint = CellInt;	// error
	ReturnFunction(badrefint);
}

void CellAny(any value) {}
void CellInt(int value) {}
void RefAny(any &value) {}
void RefInt(int &value) {}

Function ReturnFunction(Function func)
{
	return func;
}
