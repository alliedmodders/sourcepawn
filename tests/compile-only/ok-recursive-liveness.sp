

void initPlayerData(int i) {
    if (i < 5)
        initPlayerData(i++);
}
void initAllPlayerData() {
    initPlayerData(0);
}
void initStructures() {
    initAllPlayerData();
}

public void main() {
    initStructures();
}
