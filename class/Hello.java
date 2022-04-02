public class Hello implements java.io.Serializable {
    volatile int someInt = 1;
    static long someLong = 1234567890987L;
    public static void main(String[] args) {
        System.out.println("Hello");
        System.out.println(123456789);
        System.out.println(12345678.0f);
        System.out.println(123456789L);
        System.out.println(12345678.0d);
    }
}
