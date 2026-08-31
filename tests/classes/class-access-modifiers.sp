#include <shell>

class Accessor {
    int publicField;
    int privateField;

    void SetPublic(int v) {
        this.publicField = v;
    }

    void GetPublic() {
        printnum(this.publicField);
    }

    void SetPrivate(int v) {
        this.privateField = v;
    }

    void GetPrivate() {
        printnum(this.privateField);
    }

    property int PubProp {
        get() {
            return this.publicField;
        }
        set(int v) {
            this.publicField = v;
        }
    }

    property int PrivProp {
        get() {
            return this.privateField;
        }
        set(int v) {
            this.privateField = v;
        }
    }
}

public void main() {
    Accessor a = new Accessor();

    a.SetPublic(10);
    a.GetPublic();

    a.SetPrivate(20);
    a.GetPrivate();

    a.PubProp = 30;
    printnum(a.PubProp);

    a.PrivProp = 40;
    printnum(a.PrivProp);
}
