package com.wordtrellis.scala

import scala.collection.immutable.List
import scala.collection.mutable
import scala.math.Ordered

/**
  *
  * @author : Todd Cook
  *
  */
class Candidate(val cipherText: List[String],
                val decipheredText: List[String],
                var charMap: mutable.HashMap[Char, Char],
                val useHints: Boolean,
                var formula: String)
    extends Ordered[Candidate] {
  val possibleHints = new mutable.HashMap[Char, Char]()
  var score         = 0

  def this(cipherText: String, decipheredText: String) {
    this(List(cipherText), List(decipheredText), new mutable.HashMap[Char, Char](), false, "")
  }

  def this(cipherText: List[String], decipheredText: List[String]) {
    this(cipherText, decipheredText, new mutable.HashMap[Char, Char](), false, "")
  }

  def this(cipherText: String, decipheredText: List[String], charMap: mutable.HashMap[Char, Char]) {
    this(List(cipherText), decipheredText, charMap, false, "")
  }

  def this(cipherTextList: List[String],
           decipheredText: List[String],
           charMap: mutable.HashMap[Char, Char]) {
    this(cipherTextList, decipheredText, charMap, false, "")
  }

  def this() {
    this(List[String](), List[String]())
  }

  // todo replace this in calling code with the VAL reference
  def getDecipheredText: List[String] = decipheredText // .mkString(" ")

  def getDecipheredTextDisplay: String = decipheredText.mkString(" ")

  def getCipherTextDisplay: String = cipherText.mkString(" ")

  def getPossibleHints: mutable.HashMap[Char, Char] = possibleHints

  def addPossibleHints(hints: mutable.HashMap[Char, Char]): Unit = {
    hints.keysIterator.toList.foreach(k => possibleHints.put(k, hints(k)))
  }

  override def toString: String = {
    "Candidate{" +
      "cipherText='" + cipherText.mkString(" ") + '\'' +
      ", decipheredText='" + decipheredText.mkString(" ") + '\'' +
      ", formula='" + formula + '\'' +
      ", score=" + score + '}'
  }

  // Note: compare, equals and hashCode should all be similar in there tests
  def compare(that: Candidate): Int = {
    if (score > that.score) 1
    else if (score < that.score) -1
    else 0
  }

  override def equals(other: Any): Boolean = other match {
    case that: Candidate => this.decipheredText == that.decipheredText
    case _               => false
  }

  override def hashCode(): Int = decipheredText.hashCode

  def getCharMap: mutable.HashMap[Char, Char] = charMap
}
