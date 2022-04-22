class Control {
    static int[] sort(int n, int m) {
        int r[] = new int[2];
        if (n<m) { r[0] = n; r[1] = m; }
        else     { r[0] = m; r[1] = n; }
        return r;
    }
}
