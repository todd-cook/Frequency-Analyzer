
package com.wordtrellis.scala

import org.scalatest.FlatSpec


/**
  * An example of how run JUnit tests inside Scala
  *
  * @author : Todd Cook
  *
  */
class SubstitutionCipherSolverTest extends FlatSpec {

  var scg = new SubstitutionCipherSolver("")

  "SubstitutionCipherSolver" should "be able to decrypt Caesarean Ciphers" in {

    assert(getBestCandidate("FRZDUGV GLH PDQB WLPHV EHIRUH WKHLU GHDWKV")
      == "COWARDS DIE MANY TIMES BEFORE THEIR DEATHS")
    assert(getBestCandidate("WKH HYLO WKDW PHQ GR OLYHV DIWHU WKHP")
      == "THE EVIL THAT MEN DO LIVES AFTER THEM")
    assert(getBestCandidate("ESPCP TD L ETOP TYP ESP LQQLTCD ZQ XPY HSTNS ELVPY LE ESP QWZZO WPLOD ZY EZ QZCEFYP")
      == "THERE IS A TIDE INE THE AFFAIRS OF MEN WHICH TAKEN AT THE FLOOD LEADS ON TO FORTUNE")

    assert(getBestCandidate("CQSOB KOHSF WG PZIS PSQOIGS RWFH DOFHWQZSG WB HVS KOHSF FSTZSQH GIBZWUVH PIH HVS KOHSF OPGCFPG FSR OBR MSZZCK HVS UFSSBG OBR PZISG HVOH OFS ZSTH AOYS HVS RSSD PZIS CQSOB")
      == "OCEAN WATER IS BLUE BECAUSE DIRT PARTICLES IN THE WATER REFLECT SUNLIGHT BUT THE WATER ABSORBS RED AND YELLOW THE GREENS AND BLUES THAT ARE LEFT MAKE THE DEEP BLUE OCEAN")

    assert(getBestCandidate("BPM VMOWBQIBQWVA NWZ I AMBBTMUMVB WN BPM ABZQSM IZM IB IV QUXIAAM ZMKWUUMVL EM QVKZMIAM WCZ WNNMZ")
      == "THE NEGOTIATIONS FOR A SETTLEMENT OF THE STRIKE ARE AT AN IMPASSE RECOMMEND WE INCREASE OUR OFFER")
    assert(getBestCandidate("VXMDUJA JARCQVNCRL LXDUM KN LXWBRMNANM ANVJRWMNA JARCQVNCRL")
      == "MODULAR ARITHMETIC COULD BE CONSIDERED REMAINDER ARITHMETIC")

    assert(getBestCandidate("MZVYDIB DN OJ OCZ HDIY RCVO ZSZMXDNZ DN OJ OCZ WJYT")
      == "READING IS TO THE MIND WHAT EXERCISE IS TO THE BODY")

    assert(getBestCandidate("SNHPJQ NX F MJFAD XNQAJW BMNYJ RJYFQQNH JQJRJSY NY NX RFLSJYNH YFPJX F MNLM UTQNXM FSI ITJX STY YFWSNXM TW WZXY JFXNQD")
      == "NICKEL IS A HEAVY SILVER WHITE METALLIC ELEMENT IT IS MAGNETIC TAKES A HIGH POLISH AND DOES NOT TARNISH OR RUST EASILY")
  }

  def getBestCandidate(plainText: String): String = {
    scg = new SubstitutionCipherSolver(plainText)
    scg.compute()
    var result = scg.getBestCandidate.getDecipheredText.mkString("").trim
    //    println("Best Guess: " + plainText + " = " + result)
    result
  }

  /**
    * The following tests illustrate the failure using substitution based decryption
    * on random alphabet ciphers
    */


  def test_FBI_Cipher_Challenge_2008() {
    println("At this point we can see the failure using substitution based decryption on random alphabet ciphers")
    val textToDecipher = "VFWTDLCSWV. YD NSLMIJFWEJFD GSW SL NIJNQBLM FOBV EJFDVF DLNIGTFBSL. KBVBF YYY.AHB.MSK/NSCDC.OFZ FS EDF WV QLSY SA GSWI VWNNDVV. "
    println(getBestCandidate(textToDecipher))
  }


  def test_FBI_Cipher_Challenge_2007() {
    val textToDecipher = "PIKODENHFENJIKM! YIH QELB GDISBK NQB PICB. OI NI AGJ.OIL/PICB.QNT MI WB SKIW, EKC UFBEMB PIKMJCBD E PEDBBD WJNQ NQB AGJ."
    println(getBestCandidate(textToDecipher))
  }


  def test_Edgar_Allan_Poe_Goldbug_Cipher() {
    val textToDecipher = "GE JEASGDXV, ZIJ GL MW, LAAM, XZY ZMLWHFZEK EJLVDXW KWKE TX LBR ATGH LBMX AANU BAI VSMUKKSS PWN VLWK AGH GNUMK WDLNZWEG JNBXW OAEG ENWB ZWMGY MO MLW WNBX MW AL PNFDCFPKH WZKEX HSSF XKIYAHUL. MK NUM YEXDM WBXY SBC HV WYX PHWKGNAMCUK?"
    println(getBestCandidate(textToDecipher))
  }
}