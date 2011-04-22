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

package com.wordtrellis.scala

import org.scalatest.{TestFailedException, Spec}
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._

/**
 * The Spec that defines the behavior of the FrequencyAnalyzer class
 * @author : Todd Cook
 * @since : Mar 7, 2010 5:37:42 PM
 */
class FrequencyAnalyzerSpec extends Spec with AssertionsForJUnit {

    val testSentence = "the quick brown fox jumps over the lazy dog"
    val qBFLettersDescendingOrder = "OEHURTWPGVIAFNXSZDKCBQLMJY"
    var dictionary = new java.io.File(new java.io.File(".").getCanonicalPath()
                                        + java.io.File.separator + "resources"
                                        + java.io.File.separator + "words")

    def createTestFrequencyMap_Character =
      FrequencyAnalyzer.filterOutNonUpperCase(FrequencyAnalyzer.getCharFrequencies(testSentence))

    def createTestFrequencyMap_String =
      FrequencyAnalyzer.filterOutNonUpperCase(FrequencyAnalyzer.getCharFrequencies(testSentence))

    describe("A FrequencyAnalyzer object should be able to:") {

        it("reduce a text into a list of letters represented by KeyCount objects") {
            assert(createTestFrequencyMap_Character.getKeyList.size < 27)
        }

        it("reduce a text to a list of letters, ordered in descending frequency") {
            assert(qBFLettersDescendingOrder === FrequencyAnalyzer.reduceToString(testSentence))
        }

        it("provide a KeyCount list of letters no longer than the length of the alphabet") {
            assert(createTestFrequencyMap_Character.getKeyList.length === 26)
        }

        it("provide a list of letter frequencies, in descending order") {
            assert("4,3,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1"
                    === createTestFrequencyMap_Character.getValueList.mkString(","))
        }

        it("reveal how many times a particular letter appears in a text") {
            var cf = new FrequencyAnalyzer[Char]()
            assert(3 === cf.getOccurences('E', createTestFrequencyMap_Character))
        }

        it("reduce a text to a list of digraphs") {
            var dfm = FrequencyAnalyzer.digraphFrequencies(testSentence)
          assert("List(HE 2, TH 2, ZY 1, PS 1, MP 1, LA 1, IC 1, CK 1, DO 1, WN 1, JU 1, BR 1, " +
                   "OV 1, ER 1, OW 1, OX 1, QU 1, FO 1, RO 1, AZ 1, OG 1, VE 1, UI 1, UM 1)"
                   ===  dfm.getKeyCountList.toString )

        }

        it("reduce a text to a list of repeated digraphs") {
            assert("HE 2,TH 2" === FrequencyAnalyzer.getRepeatedDigraphs(testSentence).mkString(",") )
        }

        it("reduce a text to list of mirrored digraphs") {
            assert(FrequencyAnalyzer.getMirroredKeys(FrequencyAnalyzer.getDigraphKCList(testSentence)).size === 0)
            var dkcList = FrequencyAnalyzer.getMirroredKeys(FrequencyAnalyzer.getDigraphKCList(testSentence +
                    " palindromic digraphs are rare but they often indicate vowels."))
                assert("RA 2, RE 2, AR 2, LA 1, AP 1, ER 1, VO 1, OV 1, OF 1, AL 1, PA 1, FO 1"
                    ===  dkcList.mkString(", ") )
        }


        it("reduce a file to a set of digraphs") {
                    var dfm = FrequencyAnalyzer.getDigraphFrequencies(dictionary)
          // TODO think of an easier way to do this
          assert("List(81094, 77653, 56989, 56529, 54460, 54032, 53704, 51276, 48602, 48468, 47697, " +
                   "46770, 45424, 43445, 42590, 42422, 42100, 42024, 41466, 40596, 39605, 39194, " +
                   "38735, 36431, 34697, 31548, 30947, 29504, 29262, 28959, 28804, 28509, 28031, " +
                   "27230, 26956, 26204, 25711, 25487, 25168, 25124, 24931, 24587, 24479, 24118, " +
                   "24090, 23868, 23663, 23274, 22837, 22825, 22538, 22477, 22445, 22263, 22004, " +
                   "21998, 21817, 21814, 21630, 21504, 21458, 21358, 21222, 20291, 20210, 19266, " +
                   "18829, 18751, 18619, 18479, 18266, 18141, 17784, 16882, 16863, 16780, 16747, " +
                   "16429, 15782, 15607, 15367, 15268, 15199, 15174, 15146, 15141, 14870, 14779, " +
                   "14570, 14515, 13665, 13214, 12883, 12503, 12492, 12442, 12428, 12050, 12013, " +
                   "11529, 11458, 11456, 11440, 11414, 11306, 11303, 11276, 10871, 10825, 10787, " +
                   "10688, 10560, 10476, 10261, 10236, 10144, 10064, 10062, 10042, 9851, 9835, " +
                   "9797, 9764, 9657, 9600, 9584, 9560, 9384, 9187, 9166, 9127, 8969, 8920, 8852, " +
                   "8846, 8739, 8731, 8538, 8493, 8472, 8431, 8120, 8086, 8035, 7916, 7915, 7863, " +
                   "7836, 7472, 7424, 7373, 7300, 7287, 7100, 6977, 6902, 6879, 6866, 6832, 6747, " +
                   "6726, 6584, 6557, 6534, 6442, 6432, 6427, 6424, 6365, 6267, 6255, 6179, 6024, " +
                   "6009, 5989, 5814, 5791, 5601, 5547, 5500, 5480, 5394, 5278, 5256, 5189, 5187, " +
                   "5165, 5138, 5126, 5026, 4928, 4919, 4857, 4854, 4759, 4730, 4697, 4692, 4691, " +
                   "4668, 4630, 4611, 4560, 4518, 4377, 4369, 4361, 4338, 4264, 4236, 4179, 4152, " +
                   "4131, 4122, 4118, 4103, 4021, 4003, 3996, 3945, 3919, 3811, 3745, 3733, 3720, " +
                   "3708, 3644, 3591, 3555, 3506, 3486, 3477, 3387, 3252, 3241, 3217, 3206, 3183, " +
                   "3158, 3110, 3091, 3011, 2968, 2952, 2923, 2904, 2894, 2854, 2800, 2791, 2778, " +
                   "2752, 2726, 2702, 2668, 2642, 2623, 2607, 2504, 2438, 2422, 2422, 2391, 2374, " +
                   "2366, 2359, 2353, 2332, 2310, 2290, 2289, 2273, 2227, 2219, 2213, 2187, 2171, " +
                   "2145, 2107, 2091, 2054, 2051, 2005, 2003, 2000, 1983, 1980, 1921, 1903, 1896, " +
                   "1833, 1812, 1788, 1787, 1769, 1725, 1722, 1718, 1679, 1659, 1629, 1622, 1614, " +
                   "1592, 1591, 1582, 1562, 1444, 1411, 1396, 1373, 1302, 1277, 1266, 1263, 1256, " +
                   "1237, 1236, 1204, 1181, 1167, 1162, 1123, 1119, 1107, 1104, 1094, 1094, 1091, " +
                   "1079, 1078, 1059, 1058, 1036, 1033, 1016, 1016, 995, 986, 949, 926, 920, 860, " +
                   "850, 844, 816, 801, 770, 765, 748, 712, 701, 701, 697, 669, 669, 650, 646, 646, " +
                   "635, 627, 617, 615, 611, 607, 573, 567, 566, 565, 558, 555, 550, 536, 527, 517, " +
                   "515, 515, 508, 505, 503, 475, 469, 469, 469, 453, 448, 443, 429, 421, 419, 418, " +
                   "417, 403, 399, 391, 386, 383, 366, 352, 344, 343, 341, 340, 338, 329, 324, 316, " +
                   "314, 313, 312, 300, 299, 297, 296, 294, 292, 289, 287, 286, 281, 277, 271, 271, " +
                   "260, 256, 256, 255, 254, 245, 245, 241, 240, 235, 234, 231, 228, 227, 227, 225, " +
                   "225, 222, 222, 220, 215, 212, 210, 206, 201, 200, 200, 199, 197, 195, 192, 191, " +
                   "190, 181, 177, 175, 174, 174, 171, 170, 163, 162, 161, 160, 159, 152, 151, 149, " +
                   "145, 140, 136, 136, 133, 124, 123, 121, 120, 119, 116, 112, 111, 110, 106, 106, " +
                   "105, 105, 102, 99, 96, 96, 95, 94, 91, 90, 87, 85, 85, 84, 84, 83, 83, 78, 77, " +
                   "76, 75, 75, 72, 71, 69, 68, 68, 66, 65, 64, 64, 63, 63, 61, 61, 60, 59, 58, 57, " +
                   "57, 55, 54, 53, 52, 52, 49, 47, 44, 42, 42, 41, 41, 41, 40, 40, 37, 37, 36, 36, " +
                   "35, 34, 32, 30, 29, 28, 28, 27, 27, 27, 27, 27, 26, 26, 26, 26, 25, 25, 24, 24, " +
                   "23, 23, 23, 23, 22, 22, 22, 22, 22, 20, 19, 19, 18, 18, 18, 18, 18, 18, 18, 17, " +
                   "17, 17, 16, 16, 15, 15, 15, 14, 14, 14, 13, 13, 13, 13, 13, 13, 12, 11, 11, 10, " +
                   "10, 10, 10, 10, 10, 10, 9, 9, 9, 7, 7, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, " +
                   "5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, " +
                   "2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)"  === dfm.getValueList.toString)
        }

        it("reduce a file to a set of digraphs with mirror occurrences") {
            var dfm = FrequencyAnalyzer.getDigraphFrequencies(dictionary)
            dfm.getKeyCountList.foreach(x => assert(x.key.length === 2))
            assert(FrequencyAnalyzer.getMirroredKeys(dfm.getKeyCountList).size === 662)
        }

        it("produce a list of possible digraph combinations") {
            assert(FrequencyAnalyzer.getPossibleDigraphPermutations.size
                    === FrequencyAnalyzer.MAX_DIGRAPH_COMBOS)
        }

        it("produce a list of possible trigraph combinations") {
            assert(FrequencyAnalyzer.getPossibleTrigraphPermutations.size
                    === FrequencyAnalyzer.MAX_TRIGRAPH_COMBOS)
        }

        it("reduce a file to a FrequencyMap[Char]") {
            var ucfm = FrequencyAnalyzer.getCharFrequencies(dictionary)
            ucfm.getKeyCountList.foreach(x => assert(x.key.isInstanceOf[Char]))
            assert(ucfm.getKeyCountList.size === 27) // TODO check this
        }

        it("reduce a text to a FrequencyMap of trigraphs") {
            var tfm = FrequencyAnalyzer.trigraphFrequencies(testSentence)
            tfm.getKeyCountList.foreach(x => assert(x.key.length === 3))
        }

        it("reduce a file to FrequencyMap of trigraphs") {
            var utFM = FrequencyAnalyzer.getTrigraphFrequencies(dictionary)
            // number of trigraphs in unix dictionary
            assert(FrequencyAnalyzer.MAX_TRIGRAPH_COMBOS > utFM.getKeyList.size)
            assert(11957 === utFM.getKeyList.size)
        }

        it("reduce a text to a FrequencyMap of mirrored trigraphs") {
            var tfm = FrequencyAnalyzer.trigraphFrequencies("Madam, I'm adam.")
            tfm.getKeyCountList.foreach(x => assert(x.key.length === 3))
            assert(FrequencyAnalyzer.getMirroredKeys(tfm.getKeyCountList).size > 0)
        }

        it("reduce a file to FrequencyMap of mirrored trigraphs") {
            var tfm = FrequencyAnalyzer.getTrigraphFrequencies(dictionary)
            tfm.getKeyCountList.foreach(x => assert(x.key.length === 3))
            assert(FrequencyAnalyzer.getMirroredKeys(tfm.getKeyCountList).size === 9852)
        }

        it("get a list of words from a text") {
            assert(FrequencyAnalyzer.getWordList(testSentence).size === 9)
        }

        it("get a list of words from a file") {
            assert(FrequencyAnalyzer.getWordList(dictionary).size === 479829)
        }

        it("count number of consonants in a word") {
            assert(FrequencyAnalyzer.countUninterruptedConsonants("BERKSHIRE") === 4)
        }

        it("find word in a text with longest consonant grouping") {
            var result = FrequencyAnalyzer.getWordList(testSentence).map(x => new KeyCount(
              x, FrequencyAnalyzer.countUninterruptedConsonants(x)))
            result = result.sortWith(_.count > _.count)
            assert(result(0).count === 3) // "jumps", 3
        }

        it("find words in a file with a given number of sequential consonants") {
            var result = FrequencyAnalyzer.getConsonantClusterList(dictionary, 6)
            assert(result.size === 56 )
        }

        it("drop non-vowel words") {
            assert("" === FrequencyAnalyzer.dropNonvowelWord("KRP"))
            assert("RABBIT" === FrequencyAnalyzer.dropNonvowelWord("RABBIT"))
        }

        it("reduce a file to a FrequencyMap of the leading chararacters") {
            var uLeadingFM = FrequencyAnalyzer.getLeadingCharFrequencies(dictionary)
            assert("SPCAMTBUDRHENFIOGLWVKJQZYX" === uLeadingFM.getKeyList.mkString(""))
        }

        it("reduce a file to a FrequencyMap of trailing characters") {
            var uTrailingFM = FrequencyAnalyzer.getTrailingCharFrequencies(dictionary)
            // println("trailing letters:\t" + uTrailingFM.getKeyList.mkString(""))
            assert("SEDYNRTGALCMHOKIPFWUXBZVJQ" === uTrailingFM.getKeyList.mkString(""))
        }

        it("reduce a file to a FrequencyMap of word lengths") {
            var wordLengths = FrequencyAnalyzer.getWordLengths(dictionary)
            assert("9 62615,8 62334,10 54667,7 53944,11 46510,6 41699,12 37583,13 27976,5 25104," +
                     "14 19326,4 13208,15 12160,16 7137,3 6221,17 4014,18 2011,2 1271,19 1055," +
                     "20 508,21 240,22 103,1 53,23 50,24 19,25 9,27 3,29 2,26 2,28 2,30 1,31 1,45 1"
                    === wordLengths.getKeyCountList.mkString(","))
        }

        it ("reduce a file to a list of words of a given length")  {
            var result = FrequencyAnalyzer.getWordsOfLength(dictionary, 10)
            assert (result(0).length === 10)
           }

       it ("compute a dot product reduction of two probability distributions"){

            val SAD  = Map('a' ->  0.082d, 'b' -> 0.015d,'c' -> 0.028d,'d' -> 0.043d,
                           'e' -> 0.127d,'f' -> 0.022d,'g' -> 0.02d,'h' -> 0.061d,'i' -> 0.07d,
                           'j' -> 0.002d,'k' -> 0.008d,'l' -> 0.04d,'m' -> 0.024d,
                           'n' -> 0.067d,'o' -> 0.075d,'p' -> 0.019d,'q' -> 0.001d,
                           'r' -> 0.06d,'s' -> 0.063d,'t' -> 0.091d,'u' -> 0.028d,
                           'v' -> 0.01d,'w' -> 0.023d,'x' -> 0.001d,'y' -> 0.02d,'z' -> 0.001d)
            val W6 = Map ('a' -> 0.0962d,'b' -> 0d,'c' -> 0.0385d,'d' -> 0.1346d,
                          'e' -> 0d,'f'-> 0.0577d,'g' -> 0.1731d,'h' -> 0d,'i' -> 0d,
                          'j' -> 0.0577d,'k' -> 0.1346d,'l' -> 0.0577d,'m' -> 0.0192d,
                          'n' -> 0d,'o' -> 0.0192d,'p' -> 0d,'q' -> 0d,'r' -> 0d,
                          's' -> 0.0385d,'t' -> 0d,'u' -> 0.0769d,'v' -> 0d,
                          'w' -> 0.0769d,'x' -> 0d,'y' -> 0d,'z' -> 0.0192d)
          assert(0.0312532D === FrequencyAnalyzer.probabilityDotProduct(SAD, W6))
          }

  }
}