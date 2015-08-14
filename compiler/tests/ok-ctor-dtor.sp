native Handle:CreateHandle(count);
native CloseHandle(Handle:handle);

methodmap Handle {
	public Handle() = CreateHandle;
};

public main() {
	new Handle:handle = Handle(3);
}
