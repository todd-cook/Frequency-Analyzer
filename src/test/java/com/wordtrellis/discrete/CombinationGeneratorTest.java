package com.wordtrellis.discrete;


/**
 * @author : Todd Cook
 */
public class CombinationGeneratorTest {


    /**
     * N choose K:
     * the number of ways to select a sequence of k distinct objects
     * <p/>
     * N! / K! * ((N-K)!)
     * see http://en.wikipedia.org/wiki/Binomial_coefficient
     * or any Discrete Mathematics textbook
     */
    public static long nChooseK(int n, int k) {
        return factorial(n) / (factorial(k) * (factorial(n - k)));
    }

    /**
     * Naive factorial, unsuitable for large numbers of course.
     * However, note, no closed form equivalent exists for factorial;
     * there are near equivalents though, see:
     * http://www.luschny.de/math/factorial/FastFactorialFunctions.htm
     * et al.
     *
     * @param n the seed number
     * @return the factorial result
     */
    public static long factorial(int n) {
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

    public void testCombinationGenerator() {
        String[] elements = {"a", "b", "c", "d", "e", "f", "g"};
        int[] indices;
        CombinationGenerator combinationGenerator = new CombinationGenerator(elements.length, 3);
        StringBuffer combination;
        int total = 0;
        while (combinationGenerator.hasMore()) {
            combination = new StringBuffer();
            indices = combinationGenerator.getNext();
            for (int index : indices) {
                combination.append(elements[index]);
            }
            System.out.println(combination.toString());
            total++;
        }
        //  System.out.println("Total combinations: " + total + " for " + elements.length);
        // System.out.println ("== " +    factorial(7) / (factorial(3) *  ( factorial (7-3)) ));
        assert (total == nChooseK(7, 3));
    }

}
