public bool test() {
        int x;
        for (;;) {
                if (x) break; //1
                if (x) return false; //2
        }
        return true; //3
}
