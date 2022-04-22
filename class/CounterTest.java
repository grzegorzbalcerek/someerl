class CounterTest {
    public static void main(String[] args) {
        Counter c = new Counter();
        c.run();
        c.run();
        System.out.println(c.counter);
    }
}
