

package com.wordtrellis.scala

import org.scalatest.FlatSpec


/**
  * @author Todd Cook
  *
  */

class SubstitutionTest extends FlatSpec {

  val substitutionBuilder = new SubstitutionBuilder()

  "Test SubstitutionBuilder encipher" should "scramble " in {
    val text = "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG"
    println(text)
    assert(substitutionBuilder.encipher(text, 5) ==
      "YMJ VZNHP GWTBS KTC OZRUJI TAJW YMJ QFED ITL")
  }


  "Test SubstitutionBuilder decipher" should "scramble " in {
    val text = "YMJ VZNHP GWTBS KTC OZRUJI TAJW YMJ QFED ITL"
    println(text)
    assert(substitutionBuilder.decipher(text, 5) ==
      "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG")
  }
}