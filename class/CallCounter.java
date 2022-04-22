class CallCounter {
    static int callCounter() {
        Counter c = new Counter();
        Runnable r = c;
        c.run();
        r.run();
        return c.counter;
    }
}
