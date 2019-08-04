

package com.wordtrellis.scala


import org.scalatest.FunSuite

/**
  * An example of how Scala Test Fun(ction) Suite can be run via Junit
  *
  * @author : Todd Cook
  *
  */
class CryptoSuite extends FunSuite {

  val testSentence = "the quick brown fox jumps over the lazy dog"
  val qBFLettersDescendingOrder = "OEHURTWPGVIAFNXSZDKCBQLMJY"
  var dictionary: String = new java.io.File(".").getCanonicalPath +
    java.io.File.separator +
    """\resources\words"""

  def createTestFrequencyMap_String: FrequencyMap[Char] =
    FrequencyAnalyzer.filterOutNonUpperCase(FrequencyAnalyzer.getCharFrequencies(testSentence))
}
