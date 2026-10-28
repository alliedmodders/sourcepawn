enum Tag { T };
enum Other { O };

void ShowTag(Tag:x[]) {}
void ShowOther(Other:x[]) {}
void ShowInt(int:x[]) {}
void ShowI16(int16:x[]) {}
void ShowTagFixed(Tag x[3]) {}

public void main() {
    int a[6];
    Tag t[3];
    ShowTag(a);
    ShowInt(t);
    ShowOther(t);
    ShowI16(t);
    ShowTagFixed(a);
    ShowTag("hello");
}
