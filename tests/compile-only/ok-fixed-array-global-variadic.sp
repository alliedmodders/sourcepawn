native int Format(char[] buffer, int maxlength, const char[] format, any...);

char g_szThreadTitle[32][256];

public void main() {
    char szSQLQuery[512];
    Format(szSQLQuery, sizeof(szSQLQuery), "%s", g_szThreadTitle);
}
