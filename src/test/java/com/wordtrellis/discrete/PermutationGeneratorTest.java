package com.wordtrellis.discrete;


/**
 * @author : Todd Cook
 */
public class PermutationGeneratorTest {


    public static int factorial(int n) {
        int result = 1;
        for (int i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

    public void testPermutationGenerator() {
        int[] indices;
        String[] elements = {"1", "2", "3", "4", "5", "6", "7", "8"};
        PermutationGenerator permutationGenerator = new PermutationGenerator(elements.length);
        StringBuffer permutation;
        int total = 0;
        while (permutationGenerator.hasMore()) {
            permutation = new StringBuffer();
            indices = permutationGenerator.getNext();
            for (int indice : indices) {
                permutation.append(elements[indice]);
            }
            total++;
            //     System.out.println(permutation.toString());
        }
        //  System.out.println ("Total permutations: " + total + " for " +elements.length );
        assert (factorial(elements.length) == total);
    }


}
