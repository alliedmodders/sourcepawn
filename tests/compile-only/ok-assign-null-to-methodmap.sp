methodmap Panel __nullable__ {
    property Panel Child {
        public native set(Panel p);
    }
}

public void main() {
    Panel p;
    p.Child = null;
}
