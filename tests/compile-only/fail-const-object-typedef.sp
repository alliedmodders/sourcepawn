class KK {
  int m;
}

methodmap MM {
  property int val {
    public get() {
      return 1;
    }
  }
}

typedef ObjCb = function void (const KK h);
typedef MmCb = function void (const MM h);

public void main() {}
