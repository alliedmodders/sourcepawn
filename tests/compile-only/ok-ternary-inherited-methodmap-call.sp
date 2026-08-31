/* Regression: a call to an inherited methodmap method used as the
 * condition of a ternary expression must compile cleanly.
 * CheckTernaryExpr was running CheckCallExpr on the CallExpr twice --
 * once directly and again via AnalyzeForTest -- and the second pass
 * saw the implicit |this| that the first pass had rewritten into
 * call->args()[0], then tried to convert it to the first user
 * parameter's formal type:
 *   error 450: no viable conversion from 'DerivedMap' to 'char[]'
 */
#include <handles>

methodmap BaseMap < Handle {
    public BaseMap() {
        return view_as<BaseMap>(0);
    }
    public bool Get(const char[] key, int &value) {
        value = key[0];
        return true;
    }
}

methodmap DerivedMap < BaseMap {
    public DerivedMap() {
        return view_as<DerivedMap>(0);
    }

    public int Lookup(const char[] key) {
        int value;
        return (this.Get(key, value)) ? value : -1;
    }
}

// The main() below is only here to satisfy the entry-point requirement;
// the regression being tested is purely a compile-time one.
public main() {
}
