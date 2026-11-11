class KK {
  int m;

  public void Hello() {
    this.m = 1;
  }
}

methodmap MM {
  property int val {
    public get() {
      return 1;
    }
  }
  public void DoThing() {}
}

typedef KAlias = KK;

void a(const KK k) { }
void b(const KK& k) { }
void c(const KK:k) { }
void d(const MM m) { }
void e(const KAlias k) { }

public void main() {
  KK k;
  k.Hello();
  MM m;
  m.DoThing();
}
