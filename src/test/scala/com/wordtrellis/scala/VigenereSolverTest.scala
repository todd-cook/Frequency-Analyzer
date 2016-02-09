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

/**
 * @author Todd Cook
 * @since 4/19/11 9:11 PM
 */

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import collection.mutable.ListBuffer

class VigenereSolverTest extends AssertionsForJUnit {

//  val vg = new Vigenere(Vigenere.UPPER_ENGLISH)
//  val vg3 = new Vigenere(Vigenere.LOWER_ENGLISH)
  val data = new TestData()

  val textOne = "Holmes had been seated for some hours in silence with his long thin back curved over a chemical vessel in which he was brewing a particular lymal odorous product His head was sunk upon his breast and he looked from my point of view like a strange lank bird with dull grey plumage and a black top knot So watson said he suddenly you do not propose to invest in south african securities";
  val resultOne = "ocwyikoooniwugpmxwktzdwgtssayjzwyemdlbnqaaavsuwdvbrflauplooubfgqhgcscmgzlatoedcsdeidpbhtmuovpiekifpimfnoamvlpqfxejsmxmpgkccaykwfzpyuavtelwhrhmwkbbvgtguvtefjlodfefkvpxsgrsorvgtajbsauhzrzalkwuowhgedefnswmrciwcpaaavogpdnfpktdbalsisurlnpsjyeatcuceesohhdarkhwotikbroqrdfmzghgucebvgwcdqxgpbgqwlpbdaylooqdmuhbdqgmyweuik"

  val textTwo = "The method used for the preparation and reading of code messages is simple in the extreme and at the same time impossible of translation unless the key is known The ease with which the key may be changed is another point in favor of the adoption of this code by those desiring to transmit important messages without the slightest danger of their messages being read by political or business rivals etc"
  val resultTwo = "vvhqwvvrhmusgjgthkihtssejchlsfcbgvwcrlryqtfsvgahwkcuhwauglqhnslrljshbltspisprdxljsveeghlqwkasskuwepwqtwvspgoelkcqyfnsvwljsniqkgnrgybwlwgoviokhkazkqkxzgyhcecmeiujoqkwfwvefqhkijrclrlkbienqfrjljsdhgrhlsfqtwlauqrhwdmwlgusgikkflryvcwvspgpmlkassjvoqxeggveyggzmljcxxljsvpaivwikvrdrygfrjljslveggveyggeiapuuisfpbtgnwwmuczrvtwglrwugumnczvile";

  @Test
  def testEncipher2 () {
    assertEquals("OCWYIKOOONIWUGPMXWKTZDWGTSSAYJZWYEMDLBNQAAAVSUWDVBRFLAUPLOOUBFGQHGCSCMGZLATOEDCSDEIDPBHTMUOVPIEKIFPIMFNOAMVLPQFXEJSMXMPGKCCAYKWFZPYUAVTELWHRHMWKBBVGTGUVTEFJLODFEFKVPXSGRSORVGTAJBSAUHZRZALKWUOWHGEDEFNSWMRCIWCPAAAVOGPDNFPKTDBALSISURLNPSJYEATCUCEESOHHDARKHWOTIKBROQRDFMZGHGUCEBVGWCDQXGPBGQWLPBDAYLOOQDMUHBDQGMYWEUIK",
      Vigenere.encipher(textOne.replaceAll("\\s", "").toUpperCase(), "HOLMES"))
    assertEquals("VVHQWVVRHMUSGJGTHKIHTSSEJCHLSFCBGVWCRLRYQTFSVGAHWKCUHWAUGLQHNSLRLJSHBLTSPISPRDXLJSVEEGHLQWKASSKUWEPWQTWVSPGOELKCQYFNSVWLJSNIQKGNRGYBWLWGOVIOKHKAZKQKXZGYHCECMEIUJOQKWFWVEFQHKIJRCLRLKBIENQFRJLJSDHGRHLSFQTWLAUQRHWDMWLGUSGIKKFLRYVCWVSPGPMLKASSJVOQXEGGVEYGGZMLJCXXLJSVPAIVWIKVRDRYGFRJLJSLVEGGVEYGGEIAPUUISFPBTGNWWMUCZRVTWGLRWUGUMNCZVILE",
      Vigenere.encipher(textTwo.replaceAll("\\s", "").toUpperCase(), "CODES"))
  }

  @Test
  def guessKeyLength () {
    assertTrue((6, 25) == Vigenere.guessKeyLength(resultOne)(0))
    // TODO fix this, the guessed key is the second entry
    assertTrue (Vigenere.guessKeyLength(resultTwo)(1) == (5,25) )
  }

  @Test
  def guessKey(){

      val cleanText = FrequencyAnalyzer.swallowSpaces(FrequencyAnalyzer.dropNonLettersForceUpper (data.chapterText))
      val cipherText = Vigenere.encipher(cleanText, "HOLIDAY")
      println("enciphering with key: HOLIDAY")
      println(Vigenere.guessKeyLength(cipherText) )
      // Ouch, the key length is the third value, but notice the wide spread that follows after it
      //  List((14,376), (21,345), (7,325), (11,230), (24,225), (26,222), (18,211),
    val (keyLengths, occurrences) = Vigenere.guessKeyLength(cipherText).unzip
      keyLengths.foreach( x => guessCipher(cipherText , cleanText, x))
  }

  @Test
  def guessKey2(){
      val cleanText = FrequencyAnalyzer.swallowSpaces(FrequencyAnalyzer.dropNonLettersForceUpper (textOne ))
      val cipherText = Vigenere.encipher(cleanText, "HOLMES")
      println("enciphering with key: HOLMES")
      println(Vigenere.guessKeyLength(cipherText) )
      val (keyLengths, occurrences) = Vigenere.guessKeyLength(cipherText).unzip
    keyLengths.foreach( x =>   guessCipher(cipherText , cleanText, x))

  }

  def guessCipher (cipherText:String, cleanText:String, keyLength :Int)  {
      var probMaps = new ListBuffer[Map[Char, Double]]()
      (0 until keyLength).foreach(x => probMaps.append(FrequencyAnalyzer.getCharFrequencies(
                                        Vigenere.extractLetters(cipherText, x)).toProbabilityMap))
     val probabilityMaps = probMaps.toList
     /**
     * a little bit contrived, but we'll assume a perfect probability distribution map;
     * This will help us work out the kinks in the test; later we can dirty the data and
     * strain out the good from the bad
     */
     val goldenDistribution = FrequencyAnalyzer.getCharFrequencies(cleanText).toProbabilityMap
                  //Frequencies.ST_ENG_ALPHABET_DIST

    /**
     *  Frequencies.ST_ENG_ALPHABET_DIST
            top six guesses for Vigenere cipher key letter position: 0 : FEMQGKRALNUZB
            top six guesses for Vigenere cipher key letter position: 1 : FEMQGKRALNUZB
            top six guesses for Vigenere cipher key letter position: 2 : FEMGALQKRNBUS
            top six guesses for Vigenere cipher key letter position: 3 : EFQMNAGRULPSZ
            top six guesses for Vigenere cipher key letter position: 4 : EFGMRKALNQYSW
            top six guesses for Vigenere cipher key letter position: 5 : FERQNGMKASLZB
            top six guesses for Vigenere cipher key letter position: 6 : EAFNQPLMUWGRZ

                 using the frequency distribution from the cleartext yields:
            top six guesses for Vigenere cipher key letter position: 0 : FJQEUTSGPCBYI
            top six guesses for Vigenere cipher key letter position: 1 : FJQEUTSGPCBYI
            top six guesses for Vigenere cipher key letter position: 2 : FUQEJPSGTYCLB
            top six guesses for Vigenere cipher key letter position: 3 : FEQJSCBUGWPMY
            top six guesses for Vigenere cipher key letter position: 4 : FUJEQYPSGCTLA
            top six guesses for Vigenere cipher key letter position: 5 : FJQUEPCLXGMTB
            top six guesses for Vigenere cipher key letter position: 6 : FQSEGJCYUWBMP
     */
    // TODO implement a FrequencyAnalyzer Builder pattern
     var distBuffer = new ListBuffer[Map[Char, Double]] ()
     (0 to 25).toList.foreach ( x => distBuffer.append(
       FrequencyAnalyzer.shiftDistributionValues(goldenDistribution, x)  ))
     val shiftedStandardDistributions =  distBuffer.toList

    /**
     * The position match with shifted standard distribution determines the letter;
     * hence ii used for tracking
     */
     var ii =0

     (0 until keyLength).toList.foreach( x => {
                          print("top guesses for Vigenere cipher key letter position: " + x + " : ")
                          ii = 0
                          var candidates = new ListBuffer [Tuple2[Int, Double]] ()
                          shiftedStandardDistributions.foreach(y =>{
                            candidates.append( (ii, ( FrequencyAnalyzer.probabilityDotProduct(probabilityMaps(x), y) ) ))
                            ii = ii + 1
                          })
       var topEntries  = candidates.toList.sortBy(_._2) //.slice(0, 13)
        (0 until topEntries.length).foreach( x=> print( Vigenere.UPPER_ENGLISH.toList(topEntries(x)._1  )))
       println()
       println (topEntries)
             } )
    // TODO Continue to work on this section... don't give up hope yet ;-)
  }

}