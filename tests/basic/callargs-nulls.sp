#include <shell>

public float NULL_VECTOR[3] = {1.0, 2.0, 3.0};
public const char NULL_STRING[] = "asdf\n";

public void OnNullVector(const float vec[3]) {
    printfloat(vec[0]);
    printfloat(vec[1]);
    printfloat(vec[2]);
}

public void OnNullString(const char[] str) {
    print(str);
}

void main() {
    call_with_null_vector(OnNullVector);
    call_with_null_string(OnNullString);
}
