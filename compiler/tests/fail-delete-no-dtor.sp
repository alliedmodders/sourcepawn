native Handle:CreateHandle();

methodmap Handle {
	public Handle() { return CreateHandle(); }
};

public main() {
	new Handle:handle = Handle();
	delete handle;
}
