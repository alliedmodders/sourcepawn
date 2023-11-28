#include <shell>

methodmap Method < Handle
{
    public Method(int whatever) {
        return view_as<Method>(whatever);
    }
    // this will cause compiler error, and it won't report anything.
    property int m_nBugTrigger {
        // public int get() {return 1;} // if we comment this, then it will trigger error.
        public set(int val) {
            if (this.m_nBugTrigger > 5) {
            } else {
            }
        }
    }
}

public main() {
  // Just simply call it
  Method m = new Method(1);
  m.m_nBugTrigger = 1; // then error here.
}
