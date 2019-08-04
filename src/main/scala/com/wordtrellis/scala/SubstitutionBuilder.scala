
package com.wordtrellis.scala

import java.util.Date

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  *
  * @author : Todd Cook
  *
  */
class SubstitutionBuilder(val ALPHABET: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {

  val ALPHABETS2: String = ALPHABET + ALPHABET

  def encipher(plainText: String, offset: Int): String = {
    encipherSubstituted(plainText, getSubstitutionAlphabet(offset))
  }

  def getSubstitutionAlphabet(iOffset: Int): mutable.HashMap[Char, Char] = {
    val substitutionMap = new mutable.HashMap[Char, Char]()
    (0 until ALPHABET.length).foreach(x =>
      substitutionMap.put(ALPHABET.charAt(x), ALPHABETS2.charAt(x + iOffset)))
    substitutionMap
  }

  /**
    * Plain text should be converted to upper case before calling
    */
  def encipherSubstituted(plainText: String,
                          substitutionMap: mutable.HashMap[Char, Char]): String = {
    val result = new StringBuffer()
    plainText.toCharArray.toList.foreach(c => {
      val lookup = substitutionMap.get(c)
      if (lookup.isDefined) result.append(lookup.get)
      else result.append(c)
    })
    result.toString
  }

  def decipher(cipherText: String,
               offset: Int): String = {
    val trueOffset = ALPHABET.length - offset
    encipherSubstituted(cipherText, getSubstitutionAlphabet(trueOffset))
  }

  //TODO
  // private def sortListLike ( listToSort, listParadigm)
  // e.g.   listToSort random character mappings left,  listParadigm - target corpus frequency map

  /**
    * Create a random substitution alphabet, prepoulating with hints
    */
  def getRandomSubstitutionAlphabet(hints: mutable.HashMap[Char, Char])
  : mutable.HashMap[Char, Char] = {
    val random = new Random(new Date().getTime)
    val substitutionMap = new mutable.HashMap[Char, Char]()
    var availableKeys = ALPHABET.toCharArray.toList
    var availableValues = ALPHABET.toCharArray.toList
    var key: Char = ' '
    var substitutionLetter: Char = ' '

    if (hints != null) {
      hints.keysIterator.toList.foreach(key => substitutionMap.put(key, hints(key)))
      availableKeys = availableKeys diff hints.keySet.toList
      availableValues = availableValues diff hints.values.toList
    }

    (0 until availableKeys.length).toList.reverse.foreach(ii => {
      //            println("ii: "+ii)
      //            println("availableKeys : "+  availableKeys.mkString(", ") )
      //            println("availableValues : "+ availableValues.mkString(", ") )
      key = availableKeys(ii)
      substitutionLetter = availableValues(random.nextInt(availableKeys.length))
      substitutionMap.put(key, substitutionLetter)
      availableKeys = availableKeys filterNot (x => x == key)
      availableValues = availableValues filterNot (y => y == substitutionLetter)
    })
    substitutionMap
  }

  /**
    * used for getting creating substitution alphabets from the mapping of a decimated alphabet
    */
  def getSubstitutionAlphabet(iOffset: Int,
                              hm: mutable.HashMap[Char, Char]): mutable.HashMap[Char, Char] = {
    val substitutionMap = new mutable.HashMap[Char, Char]()
    var key: Char = ' '; // key
    var key2: Char = ' '; // decimated value
    var substitutionLetter: Char = ' '

    (0 to ALPHABET.length).foreach(ii => {
      key = ALPHABET.charAt(ii)
      key2 = hm(key)
      val iDecimatedStart = ALPHABET.indexOf(key2)
      substitutionLetter = ALPHABETS2.charAt(iDecimatedStart + iOffset)
      substitutionMap.put(key, substitutionLetter)
    })
    substitutionMap
  }

  /**
    * We can handle list of ciphers as well
    */
  def encipherSubstitutedList(plainTextList: List[String],
                              substitutionMap: mutable.HashMap[Char, Char])
  : List[String] = {
    val resultList = new ListBuffer[String]()
    plainTextList.foreach(plainText =>
      resultList.append(encipherSubstituted(plainText, substitutionMap)))
    resultList.toList
  }
}