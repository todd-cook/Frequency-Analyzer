package com.wordtrellis.scala

/**
  * Utility class for Vigenere Cipher
  *
  * @author Todd Cook
  *
  */

object Vigenere {

  val UPPER_ENGLISH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val CHARS: List[Char] = UPPER_ENGLISH.toList

  /**
    * Most often the key length will be the first or the first couple matches
    */
  def guessKeyLength(text: String): List[(Int, Int)] = {
    val myRange = (2 to 26).toList
    val tupleList = for (mySeed <- myRange) yield shiftCount(text, mySeed)
    tupleList.sortWith(_._2 > _._2)
  }

  /**
    * Each time the key repeats, the same character transformation occurs;
    * If the text has the same letter appearing at the same distance as the key
    * then there is a match.
    */
  def shiftCount(text: String, shift: Int): (Int, Int) = {
    var result = 0
    // shift the text off the back onto the front, so no matches are lost
    val shiftedText = shiftText(text, shift)
    var ii = 0
    text.toList.foreach(letter => {
      if (letter == shiftedText(ii)) {
        result = result + 1
      }
      ii = ii + 1;
    })
    (shift, result)
  }

  def shiftText(text: String, shift: Int): String = text.substring(text.length - shift) +
    text.substring(0, text.length - shift)

  def extractLetters(text: String, shift: Int): String = {
    if (shift == 0) return text
    val buf = new StringBuilder()
    // shift the text off the back onto the front, so no matches are lost
    var ii = shift - 1;
    val textChars = text.toList
    while (ii < text.length) {
      buf.append(textChars(ii))
      ii = ii + shift
    }
    buf.toString
  }

  def encipher(text: String, key: String): String = {
    convert(text, key.toList.map(CHARS.indexOf(_)))
  }

  private def convert(text: String, key: List[Int]): String = {
    val PLAINTEXT = text.toList.map(CHARS.indexOf(_))
    var ii = 0
    val buf = new StringBuilder()
    PLAINTEXT.foreach(letter => {
      buf.append(CHARS((key(ii % key.length) + letter) % 26))
      ii = ii + 1
    })
    buf.toString
  }

  def decipher(text: String, key: String): String = {
    convert(text, key.toList.map(CHARS.indexOf(_)).map(26 - _))
  }
}

