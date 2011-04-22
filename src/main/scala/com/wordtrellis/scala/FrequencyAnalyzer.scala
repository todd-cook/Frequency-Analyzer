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

import collection.immutable.List
import collection.mutable.{HashSet, ListBuffer, HashMap}
import io.BufferedSource
import java.io.ByteArrayInputStream

/**
 * Utility class designed for solving ciphers
 * @author : Todd Cook
 * @since : Feb 20, 2010
 *
 */
class FrequencyAnalyzer[T] {
  def getOccurences (elem: T, fm: FrequencyMap[T]): Int = {
    (fm.getKeyCountList.find(_.key == elem)).get.count
  }
}

object FrequencyAnalyzer {

  val UPPERALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  val CONSONANTS = "BCDFGHJKLMNPQRSTVWXZ"
  val VOWELS = "AEIOUY"
  val CONSONANT = "C"
  val VOWEL = "V"
  val DEFAULT_CHARSET = java.nio.charset.Charset.defaultCharset().name()

  // some useful constants
  val MAX_DIGRAPH_COMBOS = 26 * 26
  val MAX_TRIGRAPH_COMBOS = 26 * 26 * 26

  /**
   *  The ideal list of descending letter frequencies should be determined
   * by analysis of an analog of the target text.
   * However, a general default may be close enough
   * EncyclopediaBritannica_11editionVol4_usacii: ETAIONRSHDLCUFMPBGWYVKXJQZ
   * RogetThesaurus_10681-body.txt: EATIONRSLCUDHPMGFBYVWKJXQZ
   * MarkTwain_HuckleberryFinn: ETOANIHSDRLUWGMYCFBPKVJXQZ
   */
  val DEFAULT_DESCENDING_LETTER_FREQUENCIES = "ETAIONRSHDLCUFMPBGWYVKXJQZ"

  def isVowel (car: Char): Boolean = {
    VOWELS.contains(car)
  }

  def isConsonant (car: Char): Boolean = {
    CONSONANTS.contains(car)
  }

  def countCharacters (text: String): HashMap[Char, Int] = {
    getCharFrequencies(text).getKeyMap
  }

  def containsVowel (word: String): Boolean = {
    (0 until word.length).foreach(x => if (VOWELS.indexOf(word.charAt(x)) != -1) {
      return true
    })
    false
  }

  def getPossibleDigraphPermutations (): List[String] = {
    var a1 = UPPERALPHABET.split("").slice(1, 27).toList
    var a2 = UPPERALPHABET.split("").slice(1, 27).toList
    var buf = new ListBuffer[String]()
    a1.foreach(x => a2.foreach(y => {
      buf.append(x + y)
    }))
    buf.toList
  }

  def getPossibleTrigraphPermutations (): List[String] = {
    var a1 = UPPERALPHABET.split("").slice(1, 27).toList
    var a2 = UPPERALPHABET.split("").slice(1, 27).toList
    var a3 = UPPERALPHABET.split("").slice(1, 27).toList
    var buf = new ListBuffer[String]()
    a1.foreach(x => a2.foreach(y => a3.foreach(z => {
      buf.append(x + y + z)
    })))
    buf.toList
  }

  def countUninterruptedConsonants (word: String): Int = {
    var result = 0
    try {
      if (word.length == 0) {
        return 0
      }
      var hasConsonants = false;
      CONSONANTS.split("").slice(1, CONSONANTS.length + 1).toList.foreach(x => if (word.indexOf(x) != -1) {
        hasConsonants = true
      })
      if (!hasConsonants) {
        return 0
      };
      var cleanedWord = word
      VOWELS.split("").slice(1, VOWELS.length + 1).toList.foreach(x => {
        cleanedWord = cleanedWord.replaceAll(x, " ")
      })
      if (cleanedWord.length == 0) {
        return 0
      };
      cleanedWord.split(" ").toList.sortWith(_.length > _.length)(0).length
    }
    catch {
      case _: IndexOutOfBoundsException => println("word: " + word); 0
    }
  }

  /**
   * creates FrequencyMap [Character]
   * forces to upperCase
   */
  def getCharFrequencies (text: String): FrequencyMap[Char] = {
    var myf = new FrequencyMap[Char]()
    (text).toList.foreach(x => myf.add( (x.asInstanceOf[Char]).toUpper ))
    myf
  }

  def swallowSpaces (text :String) :String = text.replaceAll("\\s", "")

  /**
   * Used to compute a composite score for two FrequencyMap.toProbabilityMap frequency maps to
   */

  def probabilityDotProduct (mapOne: Map[Char, Double], mapTwo: Map[Char, Double]): Double = {
    var values = mapOne.values.zip(mapTwo.values).toList
    var newValues = values.map(x => x._1 * x._2)
    newValues.foldLeft(0d)(_.asInstanceOf[Double] + _.asInstanceOf[Double])
  }

  def shiftDistributionValues (map: Map[Char, Double], rightShift: Int): Map[Char, Double] = {
    var sortedMapList = map.toList.sortBy(_._1) // it is critical to retain alphabetical order for the shift
    val (keys, values) = sortedMapList.unzip
    var newValues = values.slice(values.length - rightShift, values.length) ++ values.slice(0, values.length - rightShift);
    keys.zip(newValues).toMap;
  }

  def filterOutNonUpperCase (fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    fm.getKeyList.foreach(x => if (UPPERALPHABET.indexOf(x) == -1) {
      fm.removeKey(x)
    })
    fm
  }

  // unit test fooder
  /*
  var fm = checkFrequencies ("the quick brown fox jumps over the lazy dog.@#$%%%%^&*^$# a bunch of stuff")
  fm = removeAllButUpperCaseAlphabet (fm)
  */
  def reduceToString (text: String): String = {
    filterOutNonUpperCase(getCharFrequencies(text)).getKeyList.mkString("")
  }

  def digraphFrequencies (text: String): FrequencyMap[java.lang.String] = {
    var digraphs = getDigraphPairs(text)
    var myf = new FrequencyMap[java.lang.String]()
    digraphs.foreach(x => myf.add(x))
    myf
  }

  def trigraphFrequencies (text: String): FrequencyMap[java.lang.String] = {
    var trigraphs = getTrigraphs(text)
    var myf = new FrequencyMap[java.lang.String]()
    trigraphs.foreach(x => myf.add(x))
    myf
  }

  def getTrigraphs (text: String): List[String] = {
    var myText = dropNonLettersForceUpper(text)
    var letters = myText.toCharArray.toList
    var letters2 = letters.slice(1, letters.size)
    var letters3 = letters.slice(2, letters.size)
    var characterPairs = letters zip letters2
    var twos = for ((x, y) <- characterPairs) yield {
      (x + "" + y)
    }
    var threes = twos zip letters3
    var trigraphs = for ((x, y) <- threes) yield {
      (x + "" + y)
    }
    trigraphs
  }

  // I suppose this will be expanded later for proper locale handling
  def forceUpper (text: String): String = text.toUpperCase

  // this does not swallow punctuation but replaces it with a space
  def dropNonLettersForceUpper (text: String): String = {
    var buffer = new StringBuilder()
    //                if (UPPERALPHABET.indexOf(x) != -1) {buffer.append(x)}
    text.toUpperCase.toCharArray.toList.foreach(x =>
                                                  if (UPPERALPHABETPLUSSPACE.indexOf(x) != -1) {
                                                    buffer.append(x)
                                                  }
                                                  else {
                                                    buffer.append(" ")
                                                  })
    buffer.toString.replaceAll("\\s+", " ") // replace multiple spaces with a single space
  }

  val UPPERALPHABETPLUSSPACE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

  // TODO consider if there's an easier way to do this; eg. get Nonletterlist... of recent dev
  def dropNonLettersForceUpperPreserveSpaces (text: String): String = {
    var buffer = new StringBuilder()
    text.toUpperCase.toCharArray.toList.foreach(x =>
                                                  if (UPPERALPHABETPLUSSPACE.indexOf(x) != -1) {
                                                    buffer.append(x)
                                                  }
                                                  else {
                                                    buffer.append(" ")
                                                  })
    buffer.toString.replaceAll("\\s+", " ") // replace multiple spaces with a single space
  }

  /**
   * Consider omitting hyphenated words when searching for consonant clusters
   */
  def dropHyphens (word: String): String = {
    if (word.indexOf("-") == -1) {
      word
    }
    else {
      ""
    }
  }

  /**
   * Consider omitting abbreviated words when searching for consonant clusters
   */
  def dropAbbreviations (word: String): String = {
    if (word.indexOf(".") == -1) {
      word
    }
    else {
      ""
    }
  }

  /**
   * Consider omitting non-vowel(?) words when searching for consonant clusters
   */
  def dropNonvowelWord (word: String): String = {
    var result = ""
    VOWELS.split("").slice(1, VOWELS.length + 1).toList.foreach(x => if (word.indexOf(x) != -1) {
      result = word
    })
    return result
  }

  def dropNonLettersForceUpperList (lines: List[String]): List[String] = {
    var buffer = new ListBuffer[String]()
    lines.foreach(x => buffer.append(dropNonLettersForceUpper(x)))
    buffer.toList
  }

  def getDigraphKCList (text: String): List[KeyCount[String]] = {
    var dpList = getDigraphPairs(text)
    var fm = new FrequencyMap[String]()
    fm.addAll(dpList)
    fm.getKeyCountList
  }

  def getDigraphPairs (text: String): List[String] = {
    //  26 * 26 = 676 maximum digraph combos
    var myText = dropNonLettersForceUpper(text)
    var letters = myText.toCharArray.toList
    var letters2 = letters.slice(1, letters.size)
    var characterPairs = letters zip letters2
    var digraphs = for ((x, y) <- characterPairs) yield {
      (x + "" + y)
    }
    digraphs.filter(_.trim.length == 2)

    //        digraphs
  }

  // load dictionary words
  // drop entries beginning or ending with a dash
  // drop obvious abbreviations: ending with a period
  // drop acronyms: letter repeated 3 times or more?
  //                word without vowels
  // drop jargon: entries with numbers

  def loadFile (pathAndFile: java.io.File): BufferedSource = {
    new BufferedSource(new java.io.FileInputStream(pathAndFile))
  }

  def processFileChars (bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      dropNonLettersForceUpper(line).toCharArray.foreach(fm.add(_))
    }
    fm
  }

  def processLeadingChars (bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      fm.add(line(0).toUpper)
    }
    filterOutNonUpperCase(fm)
  }

  def processTrailingChars (bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      fm.add(line(line.length - 1).toUpper)
    }
    filterOutNonUpperCase(fm)
  }

  def processWordLengths (bs: BufferedSource, fm: FrequencyMap[Int]): FrequencyMap[Int] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      fm.add(line.length)
    }
    fm
  }

  def processFileDigraphs (bs: BufferedSource, fm: FrequencyMap[String]): FrequencyMap[String] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      var digraphs = getDigraphPairs(line)
      digraphs.foreach(fm.add(_))
    }
    fm
  }

  def reduceToConsonantVowels (word: String): String = {
    var sb = new StringBuilder()
    dropNonLettersForceUpper(word).toCharArray.foreach(x =>
                                                         if (CONSONANTS.indexOf(x) != -1) {
                                                           sb.append(CONSONANT)
                                                         }
                                                         else {
                                                           sb.append(VOWEL)
                                                         })
    sb.toString
  }

  def processConsonantVowels (bs: BufferedSource, fm: FrequencyMap[String]): FrequencyMap[String] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      fm.add(reduceToConsonantVowels(line))
    }
    fm
  }

  def processFileTrigraphs (bs: BufferedSource, fm: FrequencyMap[String])
  : FrequencyMap[String] = {
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      var trigraphs = getTrigraphs(line)
      trigraphs.foreach(fm.add(_))
    }
    fm
  }

  def getCharFrequencies (pathAndFile: java.io.File): FrequencyMap[Char] = {
    processFileChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def getDigraphFrequencies (pathAndFile: java.io.File): FrequencyMap[String] = {
    var digraphsFM = new FrequencyMap[String]()
    processFileDigraphs(loadFile(pathAndFile), digraphsFM)
  }

  /**
   * Maximum number of trigraph combinations: 26*26*26 = 17576
   */
  def getTrigraphFrequencies (pathAndFile: java.io.File): FrequencyMap[String] = {
    var trigraphsFM = new FrequencyMap[String]()
    processFileTrigraphs(loadFile(pathAndFile), trigraphsFM)
  }

  def getLeadingCharFrequencies (pathAndFile: java.io.File): FrequencyMap[Char] = {
    processLeadingChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def getTrailingCharFrequencies (pathAndFile: java.io.File): FrequencyMap[Char] = {
    processTrailingChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def getWordLengths (pathAndFile: java.io.File): FrequencyMap[Int] = {
    processWordLengths(loadFile(pathAndFile), new FrequencyMap[Int]())
  }

  def getWordList (pathAndFile: java.io.File): List[String] = {
    getWords(loadFile(pathAndFile))
  }

  def getWordList (text: String): List[String] = {
    splitLineList(getWords(getSourceFromString(text)), " ")
  }

  def splitLineList (lines: List[String], splitOn: String): List[String] = {
    var buffer = new ListBuffer[String]()
    lines.foreach(line => line.split(" ").foreach(word => buffer.append(word)))
    buffer.toList
  }

  def getConsonantClusterList (pathAndFile: java.io.File, numberOfConsonants: Int)
  : List[String] = {
    var bs = loadFile(pathAndFile)
    var buffer = new ListBuffer[String]()
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = dropNonLettersForceUpper(linesIt.next)
      if (countUninterruptedConsonants(line) == numberOfConsonants) {
        buffer.append(forceUpper(line))
      }
    }
    buffer.toList
  }

  def getWordsOfLength (pathAndFile: java.io.File, numberOfLetters: Int)
  : List[String] = {
    var bs = loadFile(pathAndFile)
    var buffer = new ListBuffer[String]()
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = dropNonLettersForceUpper(linesIt.next)
      // we skip words with a space in them because punctuation above
      // has replaced spaces
      if (line.length == numberOfLetters && line.indexOf(" ") == -1) {
        buffer.append(forceUpper(line))
      }
    }
    buffer.toList.removeDuplicates
  }

  def getWords (bs: BufferedSource): List[String] = {
    var buffer = new ListBuffer[String]()
    var linesIt = bs.getLines()
    while (linesIt.hasNext) {
      var line = linesIt.next
      buffer.append(forceUpper(line))
    }
    buffer.toList
  }

  def getConsonantVowelFrequencies (pathAndFile: java.io.File): FrequencyMap[String] = {
    var cvFM = new FrequencyMap[String]()
    processConsonantVowels(loadFile(pathAndFile), cvFM)
  }

  def getSourceFromString (text: String): BufferedSource = {
    new BufferedSource(new ByteArrayInputStream(text.getBytes(DEFAULT_CHARSET)))
  }

  /**
   * Useful for downsizing a list of large frequencies to something proportionate
   */
  def ratioize (intTuples: List[(Int, Int)]): List[Double] = {
    for ((x, y) <- intTuples) yield {
      (x.asInstanceOf[Double] / y)
    }
  }

  def getRepeatedDigraphs (text: String): List[KeyCount[String]] = {
    getDigraphKCList(text).filter(_.count > 1)
  }

  def getMirroredKeys (kcList: List[KeyCount[String]]): List[KeyCount[String]] = {
    kcList.filter(x => kcList.contains(new KeyCount(x.key.reverse, 0)))
  }

  /**
   * This function assumes the incoming HashMap
   * is a pair of unique keys and values,
   * representing alphabet keys and decimated values, respectively
   *
   * @param hm
   * @return
   */
  def invertMap (hm: HashMap[Char, Char])  :HashMap[Char, Char] = {
    var invertedHM = new HashMap[Char, Char]
    hm.keys.foreach(x => invertedHM.put(hm.get(x).get, x))
    invertedHM
  }

  def mapToHashString (hm: HashMap[Char, Char]): String = {
    hm.keysIterator.toList.mkString("") +
      hm.valuesIterator.toList.mkString("")
  }

  def convertToCharacterMap (source: String, dest: String): HashMap[Char, Char] = {
    buildCharacterMap(source.toCharArray.toList.asInstanceOf[List[Char]],
                      dest.toCharArray.toList.asInstanceOf[List[Char]])
  }

  // todo write test
  def buildCharacterMap (sourceChars: List[Char], destChars: List[Char]): HashMap[Char, Char] = {
    require(sourceChars.length == destChars.length)
    var hm = new HashMap[Char, Char]()
    (0 to sourceChars.length - 1).foreach(ii => hm.put(sourceChars(ii), destChars(ii)))
    hm
  }

  def countOccurences (word: String, cluster: String): Int = {
    var count = 0;
    var position = 0;
    position = cluster.indexOf(word, position)
    while (position != -1) {
      count += 1
      position += 1
      position = cluster.indexOf(word, position)
    }
    count
  }

  /**
   * Takes an known cipher word and its deciphered equivalent
   * and extract the character mapping.
   * Mostly useful in testing and verification
   * @param charMap
   * @param word
   * @return
   */
  def extractMapping (charMap: HashMap[Char, Char], word: String)
  : HashMap[Char, Char] = {
    require(word.length > 0)
    var invertedMapping = invertMap(charMap)
    var mapping = new HashMap[Char, Char]()
    // the following could be more efficient by eliminating duplicate letter/calls
    // but needless optimization is the root of all evil, sayeth Knuth
    (0 to word.length - 1).foreach(ii => mapping.put(invertedMapping.get(word.charAt(ii)).get, word.charAt(ii)))
    mapping
  }

  /**
   * TODO make test for this
   */
  def extractListOfSpaces (text: String): List[Int] = {
    var buf = new ListBuffer[Int]
    (0 to text.length - 1).foreach(pos => {
      if (text.charAt(pos) == ' ') {
        buf.append(pos)
      }
    })
    buf.toList
  }

  //scala> extractListOfSpaces ("this is a bunch of shit")
  //res0: List[Int] = List(4, 7, 9, 15, 18)
  /**
   *
   */
  def insertListOfSpaces (text: String, spaceList: List[Int]): String = {
    var buf = new StringBuffer
    buf.append(text)
    spaceList.foreach(space => buf.insert(space, " "))
    buf.toString
  }

  //  scala> insertListOfSpaces ("thisisabunchofshit", List(4,7,9,15,18))
  //  res8: String = this is a bunch of shit
  /**
   * TODO make test for this
   */

  def extractNonLetterPositionList (text: String): List[Tuple2[Int, Char]] = {
    var buf = new ListBuffer[Tuple2[Int, Char]]
    (0 to text.length - 1).foreach(pos => {
      if (UPPERALPHABET.indexOf(
       (text.charAt(pos).toUpper)) == -1) {
        buf.append(Tuple2(pos, text.charAt(pos)))
      }
    })
    buf.toList
  }

  def insertNonLetterPositionList (text: String, nonLetterPositionList: List[Tuple2[Int, Char]]): String = {
    var buf = new StringBuffer
    buf.append(text)
    nonLetterPositionList.foreach(item => buf.insert(item._1, item._2))
    buf.toString
  }

  def completeLetterList (cipherFMKeys: List[Char], lettersByFrequency: List[Char]): List[Char] = {
    var buf = new ListBuffer[Char]()
    cipherFMKeys.foreach(c => buf.append(c))
    lettersByFrequency.foreach(c => if (!cipherFMKeys.contains(c)) {
      buf.append(c)
    })
    buf.toList
  }

  def extractWordList (file: java.io.File, wordLength: Int): List[String] = {

    var bs = loadFile(file)
    var hs = new HashSet[String]()
    var linesIt = bs.getLines()
    var line = ""
    try {

      while (linesIt.hasNext) {
        line = linesIt.next
        var wordList = (dropNonLettersForceUpperPreserveSpaces(linesIt.next)).split(" ")
        wordList.foreach(word => if (word.length == wordLength) {
          hs.add(word)
        })
      }
    }
    catch {
      case _: java.nio.charset.MalformedInputException => println(
        " java.nio.charset.MalformedInputException weirdness: " + line)
    }
    hs.toList
  }
}

