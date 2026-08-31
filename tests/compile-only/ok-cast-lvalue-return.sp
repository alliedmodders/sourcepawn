enum struct ScoreTracker {
    int kills;
}

ScoreTracker scores[32];

public int Test(int client) {
    return _:scores[client].kills;
}
