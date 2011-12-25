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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
/**
 * @author Todd Cook
 * @since 12/24/11 5:55 PM
 */

class VigenereTest extends AssertionsForJUnit {

  val vg = new Vigenere(Vigenere.UPPER_ENGLISH)
  val vg3 = new Vigenere(Vigenere.LOWER_ENGLISH)
  val data = new TestData()

  @Test
  def testEncipher() {
    assertEquals("TIKVMXANGVWFGFVKEYITKPTTRUCQX",
                 vg.encipher("THISISAMESSAGETHATISIMPORTANT", "ABCDEF"))
    assertEquals("BCDEFGHIJK", vg.encipher("ABCDEFGHIJ", "BBB"))
    assertEquals("OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK",
                 vg.encipher("THISISANEXAMPLEOFTHEVIGENERECIPHER", "VECTOR"))
  }

  @Test
  def testDecipher() {
    assertEquals("THISISAMESSAGETHATISIMPORTANT", vg.decipher("TIKVMXANGVWFGFVKEYITKPTTRUCQX",
                                                              "ABCDEF"))
    assertEquals("ABCDEFGHIJ", vg.decipher("BCDEFGHIJK", "BBB"))
    assertEquals("THISISANEXAMPLEOFTHEVIGENERECIPHER",
                 vg.decipher("OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK", "VECTOR"))
  }

  @Test
  def testDecipherLower() {
    assertEquals("thisisanexampleofthevigenerecipher",
                 vg3.decipher("olklwjvrgqodkpghtkcixbuviitxqzklgk", "vector"))
  }

  @Test
  def testVigenere() {
    val Key = "ABCD"
    val Plaintext = "CRYPTOISSHORTFORCRYPTOGRAPHY"
    val Ciphertext = "CSASTPKVSIQUTGQUCSASTPIUAQJB"
    val vg = new Vigenere()
    assertEquals(Ciphertext, vg.encipher(Plaintext, Key))
  }

}