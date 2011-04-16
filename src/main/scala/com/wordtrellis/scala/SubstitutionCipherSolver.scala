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

/**
 *
 * @author : Todd Cook
 * @since : Mar 6, 2010 3:54:04 PM
 */
import collection.mutable.{ListBuffer, HashMap}
import scala.collection.immutable.List
import com.wordtrellis.discrete.PermutationGenerator;


class SubstitutionCipherSolver(
        val plainText: List[String],
        val removeSpaces: Boolean,
        val removePunctuation: Boolean)
   {

    def this(plainText: String) {
        this (List(plainText), true, true)
    }

    def this (plainTextList  :List[String]) {
        // TODO
        this  ( plainTextList, true, true)
    }


    var shmp = new HashMap[Character, Character]()
    var legibilityGauge = new LegibilityGauge();
    var candidates = new ListBuffer[Candidate]()
    var hints = new HashMap[Character, Character]()

    require(plainText != null)

    def getPlainText() = plainText

    def getBestGuess() = legibilityGauge.getMostLegible(candidates.toList);

    def getBestCandidate(): Candidate = legibilityGauge.getBestCandidate(candidates.toList);

    def getRanked(): List[Candidate] = legibilityGauge.scoreCandidates(candidates.toList);

    def addCandidate(candidate: Candidate): Unit = candidates.append(candidate)

    // TODO override this for RandomSubstitutionCipherSolver
    def compute(): Unit = {
        // generate randomly substituted alphabet
        (1 to 26).foreach(ii => {
            var hm = SubstitutionBuilder.getSubstitutionAlphabet(ii);
            var c = new Candidate(plainText,
                SubstitutionBuilder.encipherSubstitutedList(plainText,
                FrequencyAnalyzer.invertMap(hm)));
            candidates.append(c);
        })
    }

    def useHints(hints: HashMap[java.lang.Character, java.lang.Character]): Unit = {
        this.hints = hints;
    }

    def computeRandomAlphabet(iterations: Int): Unit = {
        var previousCandidateAlphabets = new ListBuffer[String]();

        (1 to iterations).foreach(ii => {
            var hm = SubstitutionBuilder.getRandomSubstitutionAlphabet(hints);
            while (previousCandidateAlphabets.contains(FrequencyAnalyzer.mapToHashString(hm))) {
                hm = SubstitutionBuilder.getRandomSubstitutionAlphabet(hints);
            }
            var c = new Candidate(plainText,
                SubstitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm))
                , hm);
            candidates.append(c);
            previousCandidateAlphabets.append(FrequencyAnalyzer.mapToHashString(hm))
        })
    }


    def computeFrequencyMappedAlphabets(cipherFM: FrequencyMap[java.lang.Character],
                                        targetLanguageFM: FrequencyMap[java.lang.Character],
                                        placeHolderCombinations: Int): Unit = {
        // if the cipher is small and doesn't contain all the letters of the alphabet,
        // then we should pad it with what's available
        // we might as well use the desceding order of the target text
        var cipherLettersDesc = FrequencyAnalyzer.completeLetterList(cipherFM.getKeyList,
                                                                     targetLanguageFM.getKeyList)
        var targetLettersDesc = targetLanguageFM.getKeyList
        // create 0 to placeHolderCombinations
        var permutationGenerator = new PermutationGenerator(placeHolderCombinations)
        var permutation = new ListBuffer[java.lang.Character]()
        var changeableElements = cipherLettersDesc.slice(0, placeHolderCombinations)
        var fixedElements = cipherLettersDesc.slice(placeHolderCombinations, cipherLettersDesc.length)

        // populate with initial first mapping in the hopes of success ;-)
        var hm1 = FrequencyAnalyzer.buildCharacterMap(cipherLettersDesc, targetLettersDesc)
        var c1 = new Candidate(plainText,
            SubstitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm1))
            , hm1);
        candidates.append(c1);
        var iteration =0
        while (permutationGenerator.hasMore()) {
            permutation = new ListBuffer[java.lang.Character]()
            var indices = permutationGenerator.getNext()
            (0 to indices.length - 1).foreach(ii => permutation.append(changeableElements(indices(ii))))
            println( permutation.mkString(" "));
            var lowFrequencyLetters = fixedElements.toArray
            java.util.Collections.shuffle(java.util.Arrays.asList(lowFrequencyLetters: _*))
            permutation.appendAll(lowFrequencyLetters)
            iteration += 1
            println("checking permutation : " + iteration )
            var hm = FrequencyAnalyzer.buildCharacterMap(permutation.toList, targetLettersDesc)
            var c = new Candidate(plainText,
                SubstitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm))
                , hm);
            candidates.append(c);
        }
        println("done")
    }
}