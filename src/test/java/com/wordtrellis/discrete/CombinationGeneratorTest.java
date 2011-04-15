/*
 * Copyright (c) 2010-2011, Todd Cook.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 *     * Neither the name of the <ORGANIZATION> nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.wordtrellis.discrete;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

/**
 * @author : Todd Cook
 * @since : Mar 27, 2010 2:43:50 PM
 */
public class CombinationGeneratorTest {

    @Test
    public void testCombinationGenerator () {
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
            //       System.out.println(combination.toString());
            total++;
        }
        //  System.out.println("Total combinations: " + total + " for " + elements.length);
        // System.out.println ("== " +    factorial(7) / (factorial(3) *  ( factorial (7-3)) ));
        assertTrue(total == nChooseK(7, 3));
    }

    /**
     * N choose K:
     * the number of ways to select a sequence of k distinct objects
     * <p/>
     * N! / K! * ((N-K)!)
     * see http://en.wikipedia.org/wiki/Binomial_coefficient
     * or any Discrete Mathematics textbook
     */
    public static long nChooseK (int n, int k) {
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
    public static long factorial (int n) {
        long result = 1;
        for (int i = 1; i <= n; i++) {
            result *= i;
        }
        return result;
    }

}
