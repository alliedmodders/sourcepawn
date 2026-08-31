enum struct PlayerData {
    int client;
}

native void TestEnumStructArray(PlayerData[] players);

public void OnPluginStart() {
    PlayerData players[5];
    TestEnumStructArray(players);
}
