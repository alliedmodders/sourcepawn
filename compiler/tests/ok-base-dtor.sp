native void CloseHandle(Handle:hndl);

using __intrinsics__.Handle;

methodmap Crab < Handle {
};

public main() {
	new Crab:crab;
	delete crab;
}
