native int FileSize(const char[] path);

public void main() {
    char sFilePath[256];
    char sFileData[FileSize(sFilePath)];
}
