class Throw {
    static void verify(int n) {
        if (n < 0) throw new RuntimeException("minus");
    }
    static boolean isNegative(int n) {
        try {
            verify(n);
            return true;
        }
        catch (RuntimeException e) {
            return false;
        }
    }
}
