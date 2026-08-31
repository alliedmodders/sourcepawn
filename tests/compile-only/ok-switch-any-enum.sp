// warnings_are_errors: true

enum MyEnum {
    MyEnum_Disabled = 0,
    MyEnum_Enabled
};

void OnOptionChanged(any newValue) {
    switch (newValue) {
        case MyEnum_Disabled: {
            // No warning expected
        }
        case MyEnum_Enabled: {
            // No warning expected
        }
    }
}

public void main() {
    OnOptionChanged(MyEnum_Disabled);
}
