native void printdouble(double n);

double pinch_;

methodmap Crab {
    property double pinch {
        public get() { return pinch_; }
        public set(double value) { pinch_ = value; }
    }
};

public main()
{
    pinch_ = 10.0d;
    Crab crab;
    printdouble(crab.pinch++);
    printdouble(++crab.pinch);
    printdouble(crab.pinch--);
    printdouble(--crab.pinch);
}
