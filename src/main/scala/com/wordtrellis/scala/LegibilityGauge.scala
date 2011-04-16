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

import scala.collection.immutable.List

import scala.collection.mutable.HashSet

   /**
 *
 *
 * @author Todd Cook
* @since : Feb 20, 2010 11:28:08 AM
 */

class LegibilityGauge(val commonDigraphs: List[String],
                      val commonTrigraphs: List[String],
                      val commonWords: List[String])
  {
    // an ugly default constructor
    def this() = this (
        List("TH", "IN", "ER", "RE", "AN", "HE", "AR", "EN",
            "TI", "TE", "AT", "ON", "HA", "OU", "IT", "ES", "ST", "OR", "NT",
            "HI", "EA", "VE", "CO", "DE", "RA", "RO", "LI", "RI", "IO", "LE",
            "ND", "MA", "SE", "AL", "IC", "FO", "IL", "NE", "LA", "TA", "EL",
            "ME", "EC", "IS", "DI", "SI", "CA", "UN", "UT", "NC", "WI", "HO",
            "TR", "BE", "CE", "WH", "LL", "FI", "NO", "TO", "PE", "AS", "WA",
            "UR", "LO", "PA", "US", "MO", "OM", "AI", "PR", "WE", "AC", "EE",
            "ET", "SA", "NI", "RT", "NA", "OL", "EV", "IE", "MI", "NG", "PL",
            "IV", "PO", "CH", "EI", "AD", "SS", "IL", "OS", "UL", "EM", "NS",
            "OT", "GE", "IR", "AV", "CT", "TU", "DA", "AM", "CI", "SU", "BL",
            "OF", "BU")
        , List("THE", "ING", "AND", "ION", "ENT", "FOR", "TIO",
            "ERE", "HER", "ATE", "VER", "TER", "THA", "ATI", "HAT", "ERS",
            "HIS", "RES", "ILL", "ARE", "CON", "NCE", "ALL", "EVE", "ITH",
            "TED", "AIN", "EST", "MAN", "RED", "THI", "IVE", "REA", "WIT",
            "ONS", "ESS", "AVE", "PER", "ECT", "ONE", "UND", "INT", "ANT",
            "HOU", "MEN", "WAS", "OUN", "PRO", "STA", "INE", "WHI", "OVE",
            "TIN", "AST", "DER", "OUS", "ROM", "VEN", "ARD", "EAR", "DIN",
            "STI", "NOT", "ORT", "THO", "DAY", "ORE", "BUT", "OUT", "URE",
            "STR", "TIC", "AME", "COM", "OUR", "WER", "OME", "EEN", "LAR",
            "LES", "SAN", "STE", "ANY", "ART", "NTE", "RAT", "TUR", "ICA",
            "ICH", "NDE", "PRE", "ENC", "HAS", "WHE", "WIL", "ERA", "LIN",
            "TRA")
        , List("THE",
            "OF",
            "AND ",
            "TO",
            //"A",
            "IN",
            "THAT",
            "IS ",
            //"I",
            "IT", "FOR", "AS", "WITH", "WAS", "HIS", "HE", "BE",
            "NOT", "BY", "BUT", "HAVE", "YOU", "WHICH", "ARE", "ON", "OR",
            "HER", "HAD", "AT", "FROM", "THIS", "MY", "THEY", "ALL", "THEIR",
            "AN", "SHE", "HAS", "WERE", "ME", "BEEN", "HIM", "ONE", "SO", "IF",
            "WILL", "THERE", "WHO", "NO", "WE", "WHEN", "WHAT", "YOUR", "MORE",
            "WOULD", "THEM", "SOME", "THAN", "MAY", "UPON", "ITS", "OUT",
            "INTO", "OUR", "THESE", "MAN", "UP", "DO", "LIKE", "SHALL",
            "GREAT", "NOW", "SUCH", "SHOULD", "OTHER", "ONLY", "ANY", "THEN",
            "ABOUT", "THOSE", "CAN", "MADE", "WELL", "OLD", "MUST", "US",
            "SAID", "TIME", "EVEN", "NEW", "COULD", "VERY", "MUCH", "OWN",
            "MOST", "MIGHT", "FIRST", "AFTER", "YET", "TWO")
        )

    var dictionary = new HashSet[String]();

    def loadDictionary(file: java.io.File): Unit = {
        var words = FrequencyAnalyzer.getWordList(file)
        words.foreach(word => dictionary.add(word))
        //todo implement filtering out small lengths
        //             this.words = uw.getWordsOfLengthOrGreater(3); //4);
        //        this.words = FrequencyAnalyzer.forceUpper(words);
    }

    def scoreCandidate(c: Candidate): Int = {
        var textList = c.getDecipheredText
        var iScore = 0;
        //search for words

        textList.foreach ( text =>{
        commonWords.foreach(word => iScore += 6 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
        commonDigraphs.foreach(word => iScore += 4 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
        commonTrigraphs.foreach(word => iScore += 8 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
        dictionary.foreach(word => {
            var count = FrequencyAnalyzer.countOccurences(word, text)
            iScore += 10 * count * word.length
            if (count != 0 && word.length >= 7) {
                c.addPossibleHints(FrequencyAnalyzer.extractMapping(c.getCharMap(), word));
            }
        })
        })
        iScore
    }

    def scoreCandidates(candidates: List[Candidate]): List[Candidate] = {
        candidates.foreach(c => c.score = scoreCandidate(c))
        candidates.sortWith(_ > _)
    }

    def getBestCandidate(candidates: List[Candidate]): Candidate = {
        scoreCandidates(candidates)(0)
    }

    def getMostLegible(candidates: List[Candidate]): String = {
        scoreCandidates(candidates)(candidates.size - 1).toString
    }
}