package com.wordtrellis.scala

import org.scalatest.FlatSpec

/**
  * @author Todd Cook
  *
  */
class VigenereTest extends FlatSpec {
  //
  //  val vg = new Vigenere(Vigenere.UPPER_ENGLISH)
  //  val vg3 = new Vigenere(Vigenere.LOWER_ENGLISH)
  val data = new TestData()

  "Test Vigenere encipher" should "scramble " in {

    assert(
      "TIKVMXANGVWFGFVKEYITKPTTRUCQX" ==
        Vigenere.encipher("THISISAMESSAGETHATISIMPORTANT", "ABCDEF"))
    assert("BCDEFGHIJK" == Vigenere.encipher("ABCDEFGHIJ", "BBB"))
    assert(
      "OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK" ==
        Vigenere.encipher("THISISANEXAMPLEOFTHEVIGENERECIPHER", "VECTOR"))
  }

  "Test Vigenere decipher" should "descramble " in {
    assert(
      "THISISAMESSAGETHATISIMPORTANT" ==
        Vigenere.decipher("TIKVMXANGVWFGFVKEYITKPTTRUCQX", "ABCDEF"))
    assert("ABCDEFGHIJ" == Vigenere.decipher("BCDEFGHIJK", "BBB"))
    assert(
      "THISISANEXAMPLEOFTHEVIGENERECIPHER" ==
        Vigenere.decipher("OLKLWJVRGQODKPGHTKCIXBUVIITXQZKLGK", "VECTOR"))
  }

  "Test Vigenere" should "scramble " in {
    val Key        = "ABCD"
    val Plaintext  = "CRYPTOISSHORTFORCRYPTOGRAPHY"
    val Ciphertext = "CSASTPKVSIQUTGQUCSASTPIUAQJB"

    assert(Ciphertext == Vigenere.encipher(Plaintext, Key))
  }

}
