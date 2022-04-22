class Calculator {
    public static void main(String[] args) {
        Calculator c = new Calculator();
        c.verifyArgs(args);
        int[] numbers = c.convert(args);
        System.out.println("compute result: " +
            CalculatorCompute.compute(numbers[0],numbers[1]));
    }
    public void verifyArgs(String[] args) {
        if (args.length < 2)
            throw new RuntimeException("Not enough arguments");
    }
    public int[] convert(String[] args) {
        int[] results = new int[args.length];
        for (int j=0; j<args.length; j++)
            try {
                results[j] = Integer.parseInt(args[j]);
            }
            catch (NumberFormatException e) {
                results[j] = 0;
            }
        return results;
    }
}
