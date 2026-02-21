native void printnum64(int64 a);

int64 pinch_;

methodmap Crab {
    property int64 pinch {
        public get() { return pinch_; }
        public set(int64 value) { pinch_ = value; }
    }
};

public main()
{
    pinch_ = 100;
    Crab crab;
    printnum64(crab.pinch++);
    printnum64(++crab.pinch);
    printnum64(crab.pinch--);
    printnum64(--crab.pinch);
}
