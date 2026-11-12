native void printnum64(int64 a);

int64 pinch_;

methodmap Crab {
    static property int64 pinch {
        public get() { return pinch_; }
        public set(int64 value) { pinch_ = value; }
    }
};

public main()
{
    pinch_ = 100;
    printnum64(Crab.pinch++);
    printnum64(++Crab.pinch);
    printnum64(Crab.pinch--);
    printnum64(--Crab.pinch);

    Crab.pinch += 5;
    printnum64(Crab.pinch);
}
