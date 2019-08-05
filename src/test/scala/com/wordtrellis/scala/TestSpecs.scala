package com.wordtrellis.scala

import org.scalatest.FlatSpec

/**
  * To run several Scala Test Specs, you need a class set up like this.
  * However, if there is an Exception in the Spec,
  * it will not fail the JUnit test--so do carefully
  * read the specification it prints out.
  *
  * @author : Todd Cook
  *
  */
class TestSpecs extends FlatSpec {

  def testKeyCount() {
    new KeyCountSpec().execute()
  }

  def testFrequencyMap() {
    new FrequencyMapSpec().execute()
  }

  def testCryptoFrequencies() {
    new FrequencyAnalyzerSpec().execute()
  }

}
