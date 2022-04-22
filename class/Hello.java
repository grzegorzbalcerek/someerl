public class Hello implements java.io.Serializable {
    volatile int someInt = 1;
    static long someLong = 1234567890987L;
    final static char A = 'A';
    public static void main(String[] args) {
        int n = 10;
        int k = 0;
        Hello hello = new Hello();
        System.out.println("Hello");
        System.out.println(123456789 + hello.someInt - 7* hello.someInt);
        for (int j = 0; j < n; j++) {
            k += j;
            k--;
        }
        try {
            if (A == 'A' && n == k) {
                System.out.println(12345678.0f);
                throw new RuntimeException("ex");
            }
        } catch (RuntimeException e) {
            System.out.println("e: " + e);
        } finally {
            System.out.println("finally");
        }
        System.out.println(123456789L + someLong);
        System.out.println(12345678.0d);
    }
}
