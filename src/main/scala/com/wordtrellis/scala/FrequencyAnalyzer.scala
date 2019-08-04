
package com.wordtrellis.scala

import java.io.ByteArrayInputStream

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

/**
  * Utility class designed for solving ciphers
  *
  * @author : Todd Cook
  *
  *
  */
class FrequencyAnalyzer[T] {
  def getOccurences(elem: T, fm: FrequencyMap[T]): Int = {
    fm.getKeyCountList.find(_.key == elem).get.count
  }
}

object FrequencyAnalyzer {

  val UPPERALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  val UPPERALPHABETPLUSSPACE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

  val alphabetsPlusSpace: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ " + "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toLowerCase()

  val CONSONANTS = "BCDFGHJKLMNPQRSTVWXZ"
  val VOWELS = "AEIOUY"
  val CONSONANT = "C"
  val VOWEL = "V"
  val DEFAULT_CHARSET: String = java.nio.charset.Charset.defaultCharset().name()

  // some useful constants
  val MAX_DIGRAPH_COMBOS: Int = 26 * 26
  val MAX_TRIGRAPH_COMBOS: Int = 26 * 26 * 26

  /**
    * The ideal list of descending letter frequencies should be determined
    * by analysis of an analog of the target text.
    * However, a general default may be close enough
    * EncyclopediaBritannica_11editionVol4_usacii: ETAIONRSHDLCUFMPBGWYVKXJQZ
    * RogetThesaurus_10681-body.txt: EATIONRSLCUDHPMGFBYVWKJXQZ
    * MarkTwain_HuckleberryFinn: ETOANIHSDRLUWGMYCFBPKVJXQZ
    */
  val DEFAULT_DESCENDING_LETTER_FREQUENCIES = "ETAIONRSHDLCUFMPBGWYVKXJQZ"

  def isVowel(car: Char): Boolean = {
    VOWELS.contains(car)
  }

  def isConsonant(car: Char): Boolean = {
    CONSONANTS.contains(car)
  }

  def countCharacters(text: String): mutable.HashMap[Char, Int] = {
    getCharFrequencies(text).getKeyMap
  }

  /**
    * creates FrequencyMap [Character]
    * forces to upperCase
    */
  def getCharFrequencies(text: String): FrequencyMap[Char] = {
    val myf = new FrequencyMap[Char]()
    text.toList.foreach(x => myf.add(x.toUpper))
    myf
  }

  def containsVowel(word: String): Boolean = {
    (0 until word.length).foreach(x => if (VOWELS.indexOf(word.charAt(x)) != -1) {
      return true
    })
    false
  }

  def getPossibleDigraphPermutations(): List[String] = {
    val a1 = UPPERALPHABET.split("").slice(1, 27).toList
    val a2 = UPPERALPHABET.split("").slice(1, 27).toList
    val buf = new ListBuffer[String]()
    a1.foreach(x => a2.foreach(y => {
      buf.append(x + y)
    }))
    buf.toList
  }

  def getPossibleTrigraphPermutations(): List[String] = {
    val a1 = UPPERALPHABET.split("").slice(1, 27).toList
    val a2 = UPPERALPHABET.split("").slice(1, 27).toList
    val a3 = UPPERALPHABET.split("").slice(1, 27).toList
    val buf = new ListBuffer[String]()
    a1.foreach(x => a2.foreach(y => a3.foreach(z => {
      buf.append(x + y + z)
    })))
    buf.toList
  }

  def swallowSpaces(text: String): String = text.replaceAll("\\s", "")

  /**
    * Used to compute a composite score for two FrequencyMap.toProbabilityMap frequency maps to
    */

  def probabilityDotProduct(mapOne: Map[Char, Double], mapTwo: Map[Char, Double]): Double = {
    val values = mapOne.values.zip(mapTwo.values).toList
    val newValues = values.map(x => x._1 * x._2)
    newValues.foldLeft(0d)(_ + _)
  }

  def shiftDistributionValues(map: scala.collection.Map[Char, Double], rightShift: Int): Map[Char, Double] = {
    val sortedMapList = map.toList.sortBy(_._1) // it is critical to retain alphabetical order for the shift
    val (keys, values) = sortedMapList.unzip
    val newValues = values.slice(values.length - rightShift, values.length) ++ values.slice(0, values.length - rightShift)
    keys.zip(newValues).toMap
  }

  // unit test fooder
  /*
  var fm = checkFrequencies ("the quick brown fox jumps over the lazy dog.@#$%%%%^&*^$# a bunch of stuff")
  fm = removeAllButUpperCaseAlphabet (fm)
  */
  def reduceToString(text: String): String = {
    filterOutNonUpperCase(getCharFrequencies(text)).getKeyList.mkString("")
  }

  def digraphFrequencies(text: String): FrequencyMap[java.lang.String] = {
    val digraphs = getDigraphPairs(text)
    val myf = new FrequencyMap[java.lang.String]()
    digraphs.foreach(x => myf.add(x))
    myf
  }

  def getDigraphPairs(text: String): List[String] = {
    //  26 * 26 = 676 maximum digraph combos
    val myText = dropNonLettersForceUpper(text)
    val letters = myText.toCharArray.toList
    val letters2 = letters.slice(1, letters.size)
    val characterPairs = letters zip letters2
    val digraphs = for ((x, y) <- characterPairs) yield { s"$x$y" }
    digraphs.filter(_.trim.length == 2)
  }

  def trigraphFrequencies(text: String): FrequencyMap[java.lang.String] = {
    val trigraphs = getTrigraphs(text)
    val myf = new FrequencyMap[java.lang.String]()
    trigraphs.foreach(x => myf.add(x))
    myf
  }

  def dropNonLetters(text: String): String = {
    val buffer = new StringBuilder()
    text.toCharArray.toList.foreach(x =>
      if (alphabetsPlusSpace.indexOf(x) != -1) {
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
  def dropHyphens(word: String): String = {
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
  def dropAbbreviations(word: String): String = {
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
  def dropNonvowelWord(word: String): String = {
    var result = ""
    VOWELS.split("").slice(1, VOWELS.length + 1).toList.foreach(x => if (word.indexOf(x) != -1) {
      result = word
    })
    result
  }

  def dropNonLettersForceUpperList(lines: List[String]): List[String] = {
    val buffer = new ListBuffer[String]()
    lines.foreach(x => buffer.append(dropNonLettersForceUpper(x)))
    buffer.toList
  }

  def dropSpaces(text: String): String = text.replaceAll("\\s", "")

  /**
    * This may take a while to generate, but the results are worth caching; since the results
    * indicate how many possible word lengths can be created from and initial seed letter
    */
  def getSegmentationMap(dictionaryFile: java.io.File): Map[Char, List[Int]] = {
    val bs = loadFile(dictionaryFile)
    val linesIt = bs.getLines()
    val segmentationMap = new mutable.HashMap[Char, List[Int]]()
    while (linesIt.hasNext) {
      val line = dropNonLettersForceUpper(linesIt.next)
      if (!segmentationMap.contains(line(0))) {
        segmentationMap.put(line(0), List(line.length))
      } else {
        val tmpList = segmentationMap(line(0))
        if (!tmpList.contains(line.length)) {
          segmentationMap.put(line(0), tmpList ::: List(line.length()))
        }
      }
    }
    segmentationMap.remove(' ')
    segmentationMap.toMap
  }

  def getCharFrequencies(pathAndFile: java.io.File): FrequencyMap[Char] = {
    processFileChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def processFileChars(bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      dropNonLettersForceUpper(line).toCharArray.foreach(fm.add)
    }
    fm
  }

  def getDigraphFrequencies(pathAndFile: java.io.File): FrequencyMap[String] = {
    val digraphsFM = new FrequencyMap[String]()
    processFileDigraphs(loadFile(pathAndFile), digraphsFM)
  }

  def processFileDigraphs(bs: BufferedSource, fm: FrequencyMap[String]): FrequencyMap[String] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      val digraphs = getDigraphPairs(line)
      digraphs.foreach(fm.add)
    }
    fm
  }

  /**
    * Maximum number of trigraph combinations: 26*26*26 = 17576
    */
  def getTrigraphFrequencies(pathAndFile: java.io.File): FrequencyMap[String] = {
    val trigraphsFM = new FrequencyMap[String]()
    processFileTrigraphs(loadFile(pathAndFile), trigraphsFM)
  }

  def processFileTrigraphs(bs: BufferedSource, fm: FrequencyMap[String])
  : FrequencyMap[String] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      val trigraphs = getTrigraphs(line)
      trigraphs.foreach(fm.add)
    }
    fm
  }

  // load dictionary words
  // drop entries beginning or ending with a dash
  // drop obvious abbreviations: ending with a period
  // drop acronyms: letter repeated 3 times or more?
  //                word without vowels
  // drop jargon: entries with numbers

  def getTrigraphs(text: String): List[String] = {
    val myText = dropNonLettersForceUpper(text)
    val letters = myText.toCharArray.toList
    val letters2 = letters.slice(1, letters.size)
    val letters3 = letters.slice(2, letters.size)
    val characterPairs = letters zip letters2
    val twos = for ((x, y) <- characterPairs) yield { s"$x$y" }
    val threes = twos zip letters3
    val trigraphs = for ((x, y) <- threes) yield {s"$x$y" }
    trigraphs
  }

  def getLeadingCharFrequencies(pathAndFile: java.io.File): FrequencyMap[Char] = {
    processLeadingChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def processLeadingChars(bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      fm.add(line(0).toUpper)
    }
    filterOutNonUpperCase(fm)
  }

  def getTrailingCharFrequencies(pathAndFile: java.io.File): FrequencyMap[Char] = {
    processTrailingChars(loadFile(pathAndFile), new FrequencyMap[Char]())
  }

  def loadFile(pathAndFile: java.io.File): BufferedSource = {
    new BufferedSource(new java.io.FileInputStream(pathAndFile))
  }

  def processTrailingChars(bs: BufferedSource, fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      fm.add(line(line.length - 1).toUpper)
    }
    filterOutNonUpperCase(fm)
  }

  def filterOutNonUpperCase(fm: FrequencyMap[Char]): FrequencyMap[Char] = {
    fm.getKeyList.foreach(x => if (UPPERALPHABET.indexOf(x) == -1) {
      fm.removeKey(x)
    })
    fm
  }

  def getWordLengths(pathAndFile: java.io.File): FrequencyMap[Int] = {
    processWordLengths(loadFile(pathAndFile), new FrequencyMap[Int]())
  }

  def processWordLengths(bs: BufferedSource, fm: FrequencyMap[Int]): FrequencyMap[Int] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      fm.add(line.length)
    }
    fm
  }

  def getWordList(pathAndFile: java.io.File): List[String] = {
    getWords(loadFile(pathAndFile))
  }

  def getWordList(text: String): List[String] = {
    splitLineList(getWords(getSourceFromString(text)), " ")
  }

  def splitLineList(lines: List[String], splitOn: String): List[String] = {
    val buffer = new ListBuffer[String]()
    lines.foreach(line => line.split(" ").foreach(word => buffer.append(word)))
    buffer.toList
  }

  def getWords(bs: BufferedSource): List[String] = {
    val buffer = new ListBuffer[String]()
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      buffer.append(forceUpper(line))
    }
    buffer.toList
  }

  def getSourceFromString(text: String): BufferedSource = {
    new BufferedSource(new ByteArrayInputStream(text.getBytes(DEFAULT_CHARSET)))
  }

  def getConsonantClusterList(pathAndFile: java.io.File, numberOfConsonants: Int)
  : List[String] = {
    val bs = loadFile(pathAndFile)
    val buffer = new ListBuffer[String]()
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = dropNonLettersForceUpper(linesIt.next)
      if (countUninterruptedConsonants(line) == numberOfConsonants) {
        buffer.append(forceUpper(line))
      }
    }
    buffer.toList
  }

  def countUninterruptedConsonants(word: String): Int = {
    try {
      if (word.length == 0) {
        return 0
      }
      var hasConsonants = false
      CONSONANTS.split("").slice(1, CONSONANTS.length + 1).toList.foreach(x => if (word.indexOf(x) != -1) {
        hasConsonants = true
      })
      if (!hasConsonants) {
        return 0
      }
      var cleanedWord = word
      VOWELS.split("").slice(1, VOWELS.length + 1).toList.foreach(x => {
        cleanedWord = cleanedWord.replaceAll(x, " ")
      })
      if (cleanedWord.length == 0) {
        return 0
      }
      cleanedWord.split(" ").toList.sortWith(_.length > _.length)(0).length
    }
    catch {
      case _: IndexOutOfBoundsException => println("word: " + word); 0
    }
  }

  // I suppose this will be expanded later for proper locale handling
  def forceUpper(text: String): String = text.toUpperCase

  // this does not swallow punctuation but replaces it with a space
  def dropNonLettersForceUpper(text: String): String = {
    val buffer = new StringBuilder()
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

  def getWordsOfLength(pathAndFile: java.io.File, numberOfLetters: Int)
  : List[String] = {
    val bs = loadFile(pathAndFile)
    val buffer = new ListBuffer[String]()
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = dropNonLettersForceUpper(linesIt.next)
      // we skip words with a space in them because punctuation above
      // has replaced spaces
      if (line.length == numberOfLetters && line.indexOf(" ") == -1) {
        buffer.append(forceUpper(line))
      }
    }
    buffer.toList.distinct
  }

  def getConsonantVowelFrequencies(pathAndFile: java.io.File): FrequencyMap[String] = {
    val cvFM = new FrequencyMap[String]()
    processConsonantVowels(loadFile(pathAndFile), cvFM)
  }

  def processConsonantVowels(bs: BufferedSource, fm: FrequencyMap[String]): FrequencyMap[String] = {
    val linesIt = bs.getLines()
    while (linesIt.hasNext) {
      val line = linesIt.next
      fm.add(reduceToConsonantVowels(line))
    }
    fm
  }

  def reduceToConsonantVowels(word: String): String = {
    val sb = new StringBuilder()
    dropNonLettersForceUpper(word).toCharArray.foreach(x =>
      if (CONSONANTS.indexOf(x) != -1) {
        sb.append(CONSONANT)
      }
      else {
        sb.append(VOWEL)
      })
    sb.toString
  }

  /**
    * Useful for downsizing a list of large frequencies to something proportionate
    */
  def ratioize(intTuples: List[(Int, Int)]): List[Double] = {
    for ((x, y) <- intTuples) yield {
      x.asInstanceOf[Double] / y
    }
  }

  def getRepeatedDigraphs(text: String): List[KeyCount[String]] = {
    getDigraphKCList(text).filter(_.count > 1)
  }

  def getDigraphKCList(text: String): List[KeyCount[String]] = {
    val dpList = getDigraphPairs(text)
    val fm = new FrequencyMap[String]()
    fm.addAll(dpList)
    fm.getKeyCountList
  }

  def getMirroredKeys(kcList: List[KeyCount[String]]): List[KeyCount[String]] = {
    kcList.filter(x => kcList.contains(new KeyCount(x.key.reverse, 0)))
  }

  def mapToHashString(hm: mutable.HashMap[Char, Char]): String = {
    hm.keysIterator.toList.mkString("") +
      hm.valuesIterator.toList.mkString("")
  }

  def convertToCharacterMap(source: String, dest: String): mutable.HashMap[Char, Char] = {
    buildCharacterMap(source.toCharArray.toList,
      dest.toCharArray.toList)
  }

  // todo write test
  def buildCharacterMap(sourceChars: List[Char], destChars: List[Char]): mutable.HashMap[Char, Char] = {
    require(sourceChars.length == destChars.length)
    val hm = new mutable.HashMap[Char, Char]()
    (0 until sourceChars.length).foreach(ii => hm.put(sourceChars(ii), destChars(ii)))
    hm
  }

  def countOccurences(word: String, cluster: String): Int = {
    var count = 0
    var position = 0
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
    *
    * @param charMap
    * @param word
    * @return
    */
  def extractMapping(charMap: mutable.HashMap[Char, Char], word: String)
  : mutable.HashMap[Char, Char] = {
    require(word.length > 0)
    val invertedMapping = invertMap(charMap)
    val mapping = new mutable.HashMap[Char, Char]()
    // the following could be more efficient by eliminating duplicate letter/calls
    // but needless optimization is the root of all evil, sayeth Knuth
    (0 until word.length).foreach(ii => mapping.put(invertedMapping(word.charAt(ii)), word.charAt(ii)))
    mapping
  }

  /**
    * This function assumes the incoming HashMap
    * is a pair of unique keys and values,
    * representing alphabet keys and decimated values, respectively
    *
    * @param hm
    * @return
    */
  def invertMap(hm: mutable.HashMap[Char, Char]): mutable.HashMap[Char, Char] = {
    val invertedHM = new mutable.HashMap[Char, Char]
    hm.keys.foreach(x => invertedHM.put(hm(x), x))
    invertedHM
  }

  /**
    * TODO make test for this
    */
  def extractListOfSpaces(text: String): List[Int] = {
    val buf = new ListBuffer[Int]
    (0 until text.length).foreach(pos => {
      if (text.charAt(pos) == ' ') {
        buf.append(pos)
      }
    })
    buf.toList
  }

  /**
    *
    */
  def insertListOfSpaces(text: String, spaceList: List[Int]): String = {
    val buf = new StringBuffer
    buf.append(text)
    spaceList.foreach(space => buf.insert(space, " "))
    buf.toString
  }

  //scala> extractListOfSpaces ("this is a bunch of shit")
  //res0: List[Int] = List(4, 7, 9, 15, 18)

  def extractNonLetterPositionList(text: String): List[(Int, Char)] = {
    val buf = new ListBuffer[(Int, Char)]
    (0 until text.length).foreach(pos => {
      if (UPPERALPHABET.indexOf(
        text.charAt(pos).toUpper) == -1) {
        buf.append(Tuple2(pos, text.charAt(pos)))
      }
    })
    buf.toList
  }

  def insertNonLetterPositionList(text: String, nonLetterPositionList: List[(Int, Char)]): String = {
    val buf = new StringBuffer
    buf.append(text)
    nonLetterPositionList.foreach(item => buf.insert(item._1, item._2))
    buf.toString
  }

  def completeLetterList(cipherFMKeys: List[Char], lettersByFrequency: List[Char]): List[Char] = {
    val buf = new ListBuffer[Char]()
    cipherFMKeys.foreach(c => buf.append(c))
    lettersByFrequency.foreach(c => if (!cipherFMKeys.contains(c)) {
      buf.append(c)
    })
    buf.toList
  }

  def extractWordList(file: java.io.File, wordLength: Int): List[String] = {

    val bs = loadFile(file)
    val hs = new mutable.HashSet[String]()
    val linesIt = bs.getLines()
    var line = ""
    try {

      while (linesIt.hasNext) {
        line = linesIt.next
        val wordList = dropNonLettersForceUpperPreserveSpaces(linesIt.next).split(" ")
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

  // TODO consider if there's an easier way to do this; eg. get Nonletterlist... of recent dev
  def dropNonLettersForceUpperPreserveSpaces(text: String): String = {
    val buffer = new StringBuilder()
    text.toUpperCase.toCharArray.toList.foreach(x =>
      if (UPPERALPHABETPLUSSPACE.indexOf(x) != -1) {
        buffer.append(x)
      }
      else {
        buffer.append(" ")
      })
    buffer.toString.replaceAll("\\s+", " ") // replace multiple spaces with a single space
  }
}

