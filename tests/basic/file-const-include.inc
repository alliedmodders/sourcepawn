#include <shell>

#define FILE __FILE_NAME__
#define PATH __FILE_PATH__

void PrintIncludeName() {
    print(__FILE_NAME__);
}

void PrintIncludeName2() {
    print(FILE);
}

void PrintIncludePath() {
    print(__FILE_PATH__);
}

void PrintIncludePath2() {
    print(PATH);
}

void GetIncludeName(char[] buffer, int bufferSize) {
    char fileName[] = __FILE_NAME__;
    int copySize = bufferSize < sizeof(fileName) ? bufferSize - 1 : sizeof(fileName) - 1;

    for (int i = 0; i < copySize; i++) {
        buffer[i] = fileName[i];
    }
    buffer[copySize] = 0;
}

void GetIncludePath(char[] buffer, int bufferSize) {
    char filePath[] = __FILE_PATH__;
    int copySize = bufferSize < sizeof(filePath) ? bufferSize - 1 : sizeof(filePath) - 1;

    for (int i = 0; i < copySize; i++) {
        buffer[i] = filePath[i];
    }
    buffer[copySize] = 0;
}
