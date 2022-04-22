public final class Counter
             implements Runnable {
    public int counter = 0;
    public void run() {
        counter++;
    }
}
