
package com.wordtrellis.scala

import java.io.File

import org.scalatest.FlatSpec

/**
  * @author Todd Cook
  *
  */

class FrequencyAnalyzerTest extends FlatSpec {


  def testSegmentationMap() {
    val dictionaryFile = new File(new File(".").getCanonicalPath + "/resources/words")
    val segmentationMap = FrequencyAnalyzer.getSegmentationMap(dictionaryFile)
    segmentationMap.keys.toList.sortWith(_ < _).foreach(k =>
      println(k + " : " + segmentationMap.get(k).toList.mkString(",")))
  }
}