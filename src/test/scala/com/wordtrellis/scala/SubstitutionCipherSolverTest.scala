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
 * An example of how run JUnit tests inside Scala
 * @author : Todd Cook
 * @since : Mar 7, 2010 5:22:54 PM
 */
class SubstitutionCipherSolverTest extends AssertionsForJUnit {

  var scg = new SubstitutionCipherSolver("")

  def getBestCandidate (plainText: String) :String = {
    scg = new SubstitutionCipherSolver(plainText);
    scg.compute();
    var result = scg.getBestCandidate().getDecipheredText().mkString("").trim
    println("Best Guess: " + plainText + " = " + result)
    result
  }

  @Test
  def testSubstitutionBuilder () {
    var result = getBestCandidate("FRZDUGV GLH PDQB WLPHV EHIRUH WKHLU GHDWKV")
    assertTrue(result.equals("COWARDS DIE MANY TIMES BEFORE THEIR DEATHS"))
  }

  @Test
  def testbasicTest0 () {
    var result = getBestCandidate("WKH HYLO WKDW PHQ GR OLYHV DIWHU WKHP")
    assertTrue(result.equals("THE EVIL THAT MEN DO LIVES AFTER THEM"))
  }

  @Test
  def testbasicTest1 () {
    var result = getBestCandidate("ESPCP TD L ETOP TYP ESP LQQLTCD ZQ XPY HSTNS ELVPY LE ESP QWZZO WPLOD ZY EZ QZCEFYP")
    assertTrue(result.equals("THERE IS A TIDE INE THE AFFAIRS OF MEN WHICH TAKEN AT THE FLOOD LEADS ON TO FORTUNE"))
  }

  @Test
  def testbasicTest2 () {
    var result = getBestCandidate("CQSOB KOHSF WG PZIS PSQOIGS RWFH DOFHWQZSG WB HVS KOHSF FSTZSQH GIBZWUVH PIH HVS KOHSF OPGCFPG FSR OBR MSZZCK HVS UFSSBG OBR PZISG HVOH OFS ZSTH AOYS HVS RSSD PZIS CQSOB")
    assertTrue(result.equals("OCEAN WATER IS BLUE BECAUSE DIRT PARTICLES IN THE WATER REFLECT SUNLIGHT BUT THE WATER ABSORBS RED AND YELLOW THE GREENS AND BLUES THAT ARE LEFT MAKE THE DEEP BLUE OCEAN"))
  }

  @Test
  def testbasicTest3 () {
    var result = getBestCandidate("BPM VMOWBQIBQWVA NWZ I AMBBTMUMVB WN BPM ABZQSM IZM IB IV QUXIAAM ZMKWUUMVL EM QVKZMIAM WCZ WNNMZ")
    assertTrue(result.equals("THE NEGOTIATIONS FOR A SETTLEMENT OF THE STRIKE ARE AT AN IMPASSE RECOMMEND WE INCREASE OUR OFFER"))
  }

  @Test
  def testbasicTest4 () {
    var result = getBestCandidate("VXMDUJA JARCQVNCRL LXDUM KN LXWBRMNANM ANVJRWMNA JARCQVNCRL")
    assertTrue(result.equals("MODULAR ARITHMETIC COULD BE CONSIDERED REMAINDER ARITHMETIC"))
  }

  @Test
  def testbasicTest5 () {
    var result = getBestCandidate("MZVYDIB DN OJ OCZ HDIY RCVO ZSZMXDNZ DN OJ OCZ WJYT")
    assertTrue(result.equals("READING IS TO THE MIND WHAT EXERCISE IS TO THE BODY"))
  }

  @Test
  def testbasicTest6 () {
    var result = getBestCandidate("SNHPJQ NX F MJFAD XNQAJW BMNYJ RJYFQQNH JQJRJSY NY NX RFLSJYNH YFPJX F MNLM UTQNXM FSI ITJX STY YFWSNXM TW WZXY JFXNQD")
    assertTrue(result.equals("NICKEL IS A HEAVY SILVER WHITE METALLIC ELEMENT IT IS MAGNETIC TAKES A HIGH POLISH AND DOES NOT TARNISH OR RUST EASILY"))
  }

  /**
   * The following tests illustrate the failure using substitution based decryption
   * on random alphabet ciphers
   */

  @Test
  def test_FBI_Cipher_Challenge_2008 () {
    println("At this point we can see the failure using substitution based decryption on random alphabet ciphers")
    var textToDecipher = "VFWTDLCSWV. YD NSLMIJFWEJFD GSW SL NIJNQBLM FOBV EJFDVF DLNIGTFBSL. KBVBF YYY.AHB.MSK/NSCDC.OFZ FS EDF WV QLSY SA GSWI VWNNDVV. ";
    println(getBestCandidate(textToDecipher))
  }

  @Test
  def test_FBI_Cipher_Challenge_2007 () {
    var textToDecipher = "PIKODENHFENJIKM! YIH QELB GDISBK NQB PICB. OI NI AGJ.OIL/PICB.QNT MI WB SKIW, EKC UFBEMB PIKMJCBD E PEDBBD WJNQ NQB AGJ.";
    println(getBestCandidate(textToDecipher))
  }

  @Test
  def test_Edgar_Allan_Poe_Goldbug_Cipher () {
    var textToDecipher = "GE JEASGDXV, ZIJ GL MW, LAAM, XZY ZMLWHFZEK EJLVDXW KWKE TX LBR ATGH LBMX AANU BAI VSMUKKSS PWN VLWK AGH GNUMK WDLNZWEG JNBXW OAEG ENWB ZWMGY MO MLW WNBX MW AL PNFDCFPKH WZKEX HSSF XKIYAHUL. MK NUM YEXDM WBXY SBC HV WYX PHWKGNAMCUK?"
    println(getBestCandidate(textToDecipher))
  }
}