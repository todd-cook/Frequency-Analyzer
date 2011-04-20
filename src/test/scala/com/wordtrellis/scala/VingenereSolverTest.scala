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

class VingenereSolverTest extends AssertionsForJUnit {

  val vg = new Vingenere(Vingenere.UPPER_ENGLISH)
  val vg3 = new Vingenere(Vingenere.LOWER_ENGLISH)

  @Test
  def testEncipher () {
    assertEquals("TIKVMXANGVWFGFVKEYITKPTTRUCQX", vg.encipher("THISISAMESSAGETHATISIMPORTANT", "ABCDEF"))
    assertEquals("BCDEFGHIJK", vg.encipher("ABCDEFGHIJ", "BBB"))
    assertEquals("OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK", vg.encipher("THISISANEXAMPLEOFTHEVIGENERECIPHER", "VECTOR"))
  }

  @Test
  def testDecipher () {
    assertEquals("THISISAMESSAGETHATISIMPORTANT", vg.decipher("TIKVMXANGVWFGFVKEYITKPTTRUCQX", "ABCDEF"))
    assertEquals("ABCDEFGHIJ", vg.decipher("BCDEFGHIJK", "BBB"))
    assertEquals("THISISANEXAMPLEOFTHEVIGENERECIPHER", vg.decipher("OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK", "VECTOR"))
  }

  @Test
  def testDecipherLower () {
    assertEquals("thisisanexampleofthevigenerecipher", vg3.decipher("olklwjvrgqodkpghtkcixbuviitxqzklgk", "vector"))
  }

  val textOne = "Holmes had been seated for some hours in silence with his long thin back curved over a chemical vessel in which he was brewing a particular lymal odorous product His head was sunk upon his breast and he looked from my point of view like a strange lank bird with dull grey plumage and a black top knot So watson said he suddenly you do not propose to invest in south african securities";
  val resultOne = "ocwyikoooniwugpmxwktzdwgtssayjzwyemdlbnqaaavsuwdvbrflauplooubfgqhgcscmgzlatoedcsdeidpbhtmuovpiekifpimfnoamvlpqfxejsmxmpgkccaykwfzpyuavtelwhrhmwkbbvgtguvtefjlodfefkvpxsgrsorvgtajbsauhzrzalkwuowhgedefnswmrciwcpaaavogpdnfpktdbalsisurlnpsjyeatcuceesohhdarkhwotikbroqrdfmzghgucebvgwcdqxgpbgqwlpbdaylooqdmuhbdqgmyweuik"

  val textTwo = "The method used for the preparation and reading of code messages is simple in the extreme and at the same time impossible of translation unless the key is known The ease with which the key may be changed is another point in favor of the adoption of this code by those desiring to transmit important messages without the slightest danger of their messages being read by political or business rivals etc"
  val resultTwo = "vvhqwvvrhmusgjgthkihtssejchlsfcbgvwcrlryqtfsvgahwkcuhwauglqhnslrljshbltspisprdxljsveeghlqwkasskuwepwqtwvspgoelkcqyfnsvwljsniqkgnrgybwlwgoviokhkazkqkxzgyhcecmeiujoqkwfwvefqhkijrclrlkbienqfrjljsdhgrhlsfqtwlauqrhwdmwlgusgikkflryvcwvspgpmlkassjvoqxeggveyggzmljcxxljsvpaivwikvrdrygfrjljslveggveyggeiapuuisfpbtgnwwmuczrvtwglrwugumnczvile";

  @Test
  def testEncipher2 () {
    assertEquals("ocwyikoooniwugpmxwktzdwgtssayjzwyemdlbnqaaavsuwdvbrflauplooubfgqhgcscmgzlatoedcsdeidpbhtmuovpiekifpimfnoamvlpqfxejsmxmpgkccaykwfzpyuavtelwhrhmwkbbvgtguvtefjlodfefkvpxsgrsorvgtajbsauhzrzalkwuowhgedefnswmrciwcpaaavogpdnfpktdbalsisurlnpsjyeatcuceesohhdarkhwotikbroqrdfmzghgucebvgwcdqxgpbgqwlpbdaylooqdmuhbdqgmyweuik",
                 vg3.encipher(textOne.replaceAll("\\s", "").toLowerCase(), "holmes"))
    assertEquals("vvhqwvvrhmusgjgthkihtssejchlsfcbgvwcrlryqtfsvgahwkcuhwauglqhnslrljshbltspisprdxljsveeghlqwkasskuwepwqtwvspgoelkcqyfnsvwljsniqkgnrgybwlwgoviokhkazkqkxzgyhcecmeiujoqkwfwvefqhkijrclrlkbienqfrjljsdhgrhlsfqtwlauqrhwdmwlgusgikkflryvcwvspgpmlkassjvoqxeggveyggzmljcxxljsvpaivwikvrdrygfrjljslveggveyggeiapuuisfpbtgnwwmuczrvtwglrwugumnczvile",
                 vg3.encipher(textTwo.replaceAll("\\s", "").toLowerCase(), "codes"))
  }

  @Test
  def guessKeyLength () {
    assertTrue((6, 23) == Vingenere.guessKeyLength(resultOne)(0))
    // TODO fix this, the guessed key is the second entry
    println(Vingenere.guessKeyLength(resultTwo))
    assertTrue (Vingenere.guessKeyLength(resultTwo)(1) == (5,24) )
  }

  // TODO create map for each key value, generate candidates, score, sort and assert

}