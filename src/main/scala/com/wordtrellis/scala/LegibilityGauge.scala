package com.wordtrellis.scala

import scala.collection.immutable.List
import scala.collection.mutable

/**
  *
  * @author Todd Cook
  *
  */
class LegibilityGauge(val commonDigraphs: List[String],
                      val commonTrigraphs: List[String],
                      val commonWords: List[String]) {
  val dictionary = new mutable.HashSet[String]()

  // an ugly default constructor
  // format: off
  def this() = this(
    List("TH", "IN", "ER", "RE", "AN", "HE", "AR", "EN",
      "TI", "TE", "AT", "ON", "HA", "OU", "IT", "ES", "ST", "OR", "NT",
      "HI", "EA", "VE", "CO", "DE", "RA", "RO", "LI", "RI", "IO", "LE",
      "ND", "MA", "SE", "AL", "IC", "FO", "IL", "NE", "LA", "TA", "EL",
      "ME", "EC", "IS", "DI", "SI", "CA", "UN", "UT", "NC", "WI", "HO",
      "TR", "BE", "CE", "WH", "LL", "FI", "NO", "TO", "PE", "AS", "WA",
      "UR", "LO", "PA", "US", "MO", "OM", "AI", "PR", "WE", "AC", "EE",
      "ET", "SA", "NI", "RT", "NA", "OL", "EV", "IE", "MI", "NG", "PL",
      "IV", "PO", "CH", "EI", "AD", "SS", "IL", "OS", "UL", "EM", "NS",
      "OT", "GE", "IR", "AV", "CT", "TU", "DA", "AM", "CI", "SU", "BL",
      "OF", "BU")
    , List("THE", "ING", "AND", "ION", "ENT", "FOR", "TIO",
      "ERE", "HER", "ATE", "VER", "TER", "THA", "ATI", "HAT", "ERS",
      "HIS", "RES", "ILL", "ARE", "CON", "NCE", "ALL", "EVE", "ITH",
      "TED", "AIN", "EST", "MAN", "RED", "THI", "IVE", "REA", "WIT",
      "ONS", "ESS", "AVE", "PER", "ECT", "ONE", "UND", "INT", "ANT",
      "HOU", "MEN", "WAS", "OUN", "PRO", "STA", "INE", "WHI", "OVE",
      "TIN", "AST", "DER", "OUS", "ROM", "VEN", "ARD", "EAR", "DIN",
      "STI", "NOT", "ORT", "THO", "DAY", "ORE", "BUT", "OUT", "URE",
      "STR", "TIC", "AME", "COM", "OUR", "WER", "OME", "EEN", "LAR",
      "LES", "SAN", "STE", "ANY", "ART", "NTE", "RAT", "TUR", "ICA",
      "ICH", "NDE", "PRE", "ENC", "HAS", "WHE", "WIL", "ERA", "LIN",
      "TRA")
    , List("THE", "OF", "AND ", "TO",
      //"A",
      //"I",
      "IN", "THAT", "IS ",
      "IT", "FOR", "AS", "WITH", "WAS", "HIS", "HE", "BE",
      "NOT", "BY", "BUT", "HAVE", "YOU", "WHICH", "ARE", "ON", "OR",
      "HER", "HAD", "AT", "FROM", "THIS", "MY", "THEY", "ALL", "THEIR",
      "AN", "SHE", "HAS", "WERE", "ME", "BEEN", "HIM", "ONE", "SO", "IF",
      "WILL", "THERE", "WHO", "NO", "WE", "WHEN", "WHAT", "YOUR", "MORE",
      "WOULD", "THEM", "SOME", "THAN", "MAY", "UPON", "ITS", "OUT",
      "INTO", "OUR", "THESE", "MAN", "UP", "DO", "LIKE", "SHALL",
      "GREAT", "NOW", "SUCH", "SHOULD", "OTHER", "ONLY", "ANY", "THEN",
      "ABOUT", "THOSE", "CAN", "MADE", "WELL", "OLD", "MUST", "US",
      "SAID", "TIME", "EVEN", "NEW", "COULD", "VERY", "MUCH", "OWN",
      "MOST", "MIGHT", "FIRST", "AFTER", "YET", "TWO")
  )
  // format: on

  def loadDictionary(file: java.io.File): Unit = {
    val words = FrequencyAnalyzer.getWordList(file)
    words.filter(_.length > 4).foreach(word => dictionary.add(word))
  }

  def addWords(words: Iterable[String]): Unit = words.foreach(w => dictionary.add(w))

  def scoreText(text: String): Int = {
    var iScore = 0
    commonWords.foreach(word =>
      iScore += 6 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
    commonDigraphs.foreach(word =>
      iScore += 4 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
    commonTrigraphs.foreach(word =>
      iScore += 8 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
    val words = text.split(" ")
    words.foreach(w => {
      if (dictionary.contains(w)) {
        val count = FrequencyAnalyzer.countOccurences(w, text)
        iScore += 10 * count * w.length
      }
    })
    iScore
  }

  def getBestCandidate(candidates: List[Candidate]): Candidate = {
    scoreCandidates(candidates)(0)
  }

  def getMostLegible(candidates: List[Candidate]): String = {
    scoreCandidates(candidates)(candidates.size - 1).toString()
  }

  def scoreCandidates(candidates: List[Candidate]): List[Candidate] = {
    candidates.foreach(c => c.score = scoreCandidate(c))
    candidates.sortWith(_ > _)
  }

  // TODO rewrite method was unwieldly for large dictionaries
  def scoreCandidate(c: Candidate): Int = {
    val textList = c.getDecipheredText
    var iScore   = 0
    textList.foreach(text => {
      commonWords.foreach(word =>
        iScore += 6 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
      commonDigraphs.foreach(word =>
        iScore += 4 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
      commonTrigraphs.foreach(word =>
        iScore += 8 * FrequencyAnalyzer.countOccurences(word, text) * word.length)
      //TODO
      // for each text, split into chain of potential words, from length 4 to max length of dictionary entry
      // for each potential word check to see if it's in the dictionary, if so score it
      // although this operation mushrooms the candidates quickly it should be less than the dictionary...

      dictionary.foreach(word => {
        val count = FrequencyAnalyzer.countOccurences(word, text)
        iScore += 10 * count * word.length
        if (count != 0 && word.length >= 7) {
          c.addPossibleHints(FrequencyAnalyzer.extractMapping(c.getCharMap, word))
        }
      })
    })
    iScore
  }
}
