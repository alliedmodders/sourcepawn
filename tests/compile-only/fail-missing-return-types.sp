// A missing return must be an error when the return type is not a legacy
// cell type. These functions used to emit an untyped default that failed
// the VM's verifier, surfacing as "Binary validation failed: Invalid
// instruction". Legacy cell returns keep their historical warning behavior.

enum Color {
    Red,
}

methodmap LegacyWidget {
}

class Widget {
}

typedef Callback = () -> int;

// Legacy cell types: warn, return 0.
int legacy_int() { }
bool legacy_bool() { }
float legacy_float() { }
char legacy_char() { }
any legacy_any() { }
Color legacy_enum() { }
LegacyWidget legacy_methodmap() { }

// New language features: must return a value.
int[] new_heap_array() { }
Callback new_callback() { }
Widget new_class() { }
int64 new_int64() { }
intptr new_intptr() { }
double new_double() { }
int8 new_int8() { }
int16 new_int16() { }

public void main() {
    legacy_int();
    legacy_bool();
    legacy_float();
    legacy_char();
    legacy_any();
    legacy_enum();
    legacy_methodmap();
    new_heap_array();
    new_callback();
    new_class();
    new_int64();
    new_intptr();
    new_double();
    new_int8();
    new_int16();
}
