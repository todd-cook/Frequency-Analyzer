

package com.wordtrellis.scala

import com.wordtrellis.discrete.PermutationGenerator

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Utility class for solving substitution ciphers
  *
  * @author : Todd Cook
  *
  */
class SubstitutionCipherSolver(
                                val plainText: List[String],
                                val removeSpaces: Boolean,
                                val removePunctuation: Boolean,
                                val ALPHABET: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {

  val legibilityGauge = new LegibilityGauge()
  val candidates = new ListBuffer[Candidate]()
  val substitutionBuilder = new SubstitutionBuilder(ALPHABET)
  var shmp = new mutable.HashMap[Char, Char]()
  var hints = new mutable.HashMap[Char, Char]()

  def this(plainText: String) {
    this(List(plainText), true, true)
  }

  def this(plainTextList: List[String]) {
    // TODO
    this(plainTextList, true, true)
  }

  require(plainText != null)

  def getPlainText: List[String] = plainText

  def getBestGuess: String = legibilityGauge.getMostLegible(candidates.toList)

  def getBestCandidate: Candidate = legibilityGauge.getBestCandidate(candidates.toList)

  def getRanked: List[Candidate] = legibilityGauge.scoreCandidates(candidates.toList)

  def addCandidate(candidate: Candidate): Unit = candidates.append(candidate)

  // TODO override this for RandomSubstitutionCipherSolver
  def compute(): Unit = {
    // generate randomly substituted alphabet
    (1 to 26).foreach(ii => {
      val hm = substitutionBuilder.getSubstitutionAlphabet(ii)
      val c = new Candidate(plainText,
        substitutionBuilder.encipherSubstitutedList(plainText,
          FrequencyAnalyzer.invertMap(hm)))
      candidates.append(c)
    })
  }

  def useHints(hints: mutable.HashMap[Char, Char]): Unit = {
    this.hints = hints
  }

  def computeRandomAlphabet(iterations: Int): Unit = {
    val previousCandidateAlphabets = new ListBuffer[String]()

    (1 to iterations).foreach(ii => {
      var hm = substitutionBuilder.getRandomSubstitutionAlphabet(hints)
      while (previousCandidateAlphabets.contains(FrequencyAnalyzer.mapToHashString(hm))) {
        hm = substitutionBuilder.getRandomSubstitutionAlphabet(hints)
      }
      val c = new Candidate(plainText,
        substitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm))
        , hm)
      candidates.append(c)
      previousCandidateAlphabets.append(FrequencyAnalyzer.mapToHashString(hm))
    })
  }


  def computeFrequencyMappedAlphabets(cipherFM: FrequencyMap[Char],
                                      targetLanguageFM: FrequencyMap[Char],
                                      placeHolderCombinations: Int): Unit = {
    // if the cipher is small and doesn't contain all the letters of the alphabet,
    // then we should pad it with what's available
    // we might as well use the desceding order of the target text
    val cipherLettersDesc = FrequencyAnalyzer.completeLetterList(cipherFM.getKeyList,
      targetLanguageFM.getKeyList)
    val targetLettersDesc = targetLanguageFM.getKeyList
    // create 0 to placeHolderCombinations
    val permutationGenerator = new PermutationGenerator(placeHolderCombinations)
    var permutation = new ListBuffer[Char]()
    val changeableElements = cipherLettersDesc.slice(0, placeHolderCombinations)
    val fixedElements = cipherLettersDesc.slice(placeHolderCombinations, cipherLettersDesc.length)

    // populate with initial first mapping in the hopes of success ;-)
    val hm1 = FrequencyAnalyzer.buildCharacterMap(cipherLettersDesc, targetLettersDesc)
    val c1 = new Candidate(plainText,
      substitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm1))
      , hm1)
    candidates.append(c1)
    var iteration = 0
    while (permutationGenerator.hasMore) {
      permutation = new ListBuffer[Char]()
      val indices = permutationGenerator.getNext
      indices.indices.foreach(ii => permutation.append(changeableElements(indices(ii))))
      println(permutation.mkString(" "))
      val lowFrequencyLetters = fixedElements.toArray
      java.util.Collections.shuffle(java.util.Arrays.asList(lowFrequencyLetters: _*))
      permutation.appendAll(lowFrequencyLetters)
      iteration += 1
      println("checking permutation : " + iteration)
      val hm = FrequencyAnalyzer.buildCharacterMap(permutation.toList, targetLettersDesc)
      val c = new Candidate(plainText,
        substitutionBuilder.encipherSubstitutedList(plainText, FrequencyAnalyzer.invertMap(hm))
        , hm)
      candidates.append(c)
    }
    //println("done")
  }
}