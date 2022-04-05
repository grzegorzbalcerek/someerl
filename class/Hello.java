public class Hello implements java.io.Serializable {
    volatile int someInt = 1;
    static long someLong = 1234567890987L;
    final static char A = 'A';
    public static void main(String[] args) {
        Hello hello = new Hello();
        System.out.println("Hello");
        System.out.println(123456789 + hello.someInt - 7* hello.someInt);
        if (A == 'A') {
            System.out.println(12345678.0f);
            throw new RuntimeException("ex");
        }
        System.out.println(123456789L + someLong);
        System.out.println(12345678.0d);
    }
}
