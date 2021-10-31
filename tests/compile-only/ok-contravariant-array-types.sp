typedef CallbackCell = function void (any cell);
typedef CallbackArray = function void (any array[4]);

public void OnPluginStart()
{
	ReturnCallbackCell(CellAny);	// no error
	ReturnCallbackCell(CellInt);	// no error
	
	ReturnCallbackArray(ArrayAny);	// no error
	ReturnCallbackArray(ArrayInt);	// error 100: function prototypes do not match
}

CallbackCell ReturnCallbackCell(CallbackCell callback)
{
	return callback;
}

CallbackArray ReturnCallbackArray(CallbackArray callback)
{
	return callback;
}

void CellAny(any cell) {}
void CellInt(int cell) {}
void ArrayAny(any array[4]) {}
void ArrayInt(int array[4]) {}
