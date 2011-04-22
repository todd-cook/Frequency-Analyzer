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

package com.wordtrellis.scala;

import org.scalatest.Spec

/**
 * Spec Test for FrequencyMap
 * @author : Todd Cook
 * @since : Mar 6, 2010 5:10:07 PM
 */
class FrequencyMapSpec extends Spec {

    val qBFLettersDescendingOrder = "OEHURTWPGVIAFNXSZDKCBQLMJY"
    val data = new TestData()

    def createTestFrequencyMap_Character = FrequencyAnalyzer.filterOutNonUpperCase(
                                      FrequencyAnalyzer.getCharFrequencies(data.testSentence))

    describe("A FrequencyMap object should be able to:") {

        it("reduce a text into a KeyCount list of letters") {
            assert(createTestFrequencyMap_Character.getKeyList.size < 27 )
        }

        it("reduce a text into a KeyCount list of letters, in descending order of frequency") {
            var kcList = createTestFrequencyMap_Character.getKeyCountList
            var firstPositionValue = kcList.head.count
            var lastPositionValue  = kcList.last.count
            assert(firstPositionValue > lastPositionValue)
        }

        it ("reduce a large text to a map of letters and their probability distributions") {
          val expectedResults =Map('E' -> 0.10413161338567957, 'X' -> 3.739016638624042E-4,
                                   'N' -> 0.06898485698261357, 'T' -> 0.09964479341933072,
                                  'Y' -> 0.02168629650401944, 'J' -> 0.0011217049915872126,
                                  'U' -> 0.032903346419891566, 'F' -> 0.014021312394840156,
                                  'A' -> 0.0893624976631146, 'M' -> 0.02598616563843709,
                                  'I' -> 0.06356328285660871, 'G' -> 0.023555804823331465,
                                  'V' -> 0.006917180781454478, 'L' -> 0.04112918302486446,
                                  'B' -> 0.01962983735277622, 'P' -> 0.013273509067115349,
                                  'C' -> 0.018134230697326604, 'H' -> 0.05720695457094784,
                                  'W' -> 0.036829313890446816, 'K' -> 0.010469246588147317,
                                  'R' -> 0.04150308468872686,  'O' -> 0.08786689100766498,
                                  'D' -> 0.06300243036081511, 'Z' -> 1.869508319312021E-4,
                                  'S' -> 0.058515610394466254)
          assert(expectedResults === FrequencyAnalyzer.getCharFrequencies(FrequencyAnalyzer.swallowSpaces(
            FrequencyAnalyzer.dropNonLettersForceUpper(data.chapterText))).toProbabilityMap)
         }

    }
}
