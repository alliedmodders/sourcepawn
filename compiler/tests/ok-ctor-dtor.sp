native Handle:CreateHandle(count);
native CloseHandle(Handle:handle);

methodmap Handle {
	public native Handle(int n);
};

public main() {
	new Handle:handle = Handle(3);
}
