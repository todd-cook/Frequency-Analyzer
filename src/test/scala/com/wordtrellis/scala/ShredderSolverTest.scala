package com.wordtrellis.scala

import java.io.File

import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

/**
  * Demonstration of automatically solving a complicated shredder puzzle
  *
  * @author Todd Cook
  *
  */
class ShredderSolverTest extends FlatSpec {

  "Test shredder solver" should "descramble " in {

    /**
      * This puzzle is from a popular AI class;
      * the hardest part of solving a shredder puzzle is how to represent the letters as a list of
      * strings. If spaces are given, it's easy.
      */
    val shards = new ListBuffer[Shard]()
    shards.append(new Shard(List("de", "ry", "ab", "he", "wo", " t", "ry", "hi")))
    shards.append(new Shard(List("  ", "  ", "  ", "  ", "m,", "ca", "d ", "  ")))
    shards.append(new Shard(List(" f", "is", "la", "ea", "to", " t", "un", "  ")))
    shards.append(new Shard(List("Cl", "th", "pr", "of", "yo", "wi", "Th", "in")))
    shards.append(new Shard(List("nf", "is", "od", "ho", "hi", " M", "  ", "  ")))
    shards.append(new Shard(List("ed", " b", "ge", " m", "ve", "d ", "io", "  ")))
    shards.append(new Shard(List("au", "eo", "ob", " t", "u ", "th", "eo", " t")))
    shards.append(new Shard(List(" i", "as", " m", "et", " t", " A", "n,", "  ")))
    shards.append(new Shard(List("ti", "  ", "an", "ha", "ob", "ma", "is", "  ")))
    shards.append(new Shard(List("  ", "  ", "  ", "  ", "  ", "l ", "  ", "  ")))
    shards.append(new Shard(List("ma", "f ", "s ", " t", "pr", "he", "bl", "  ")))
    shards.append(new Shard(List("ha", "wh", "is", "od", "d ", " p", "f ", "ye")))
    shards.append(new Shard(List("or", " o", "el", "ds", "s ", "at", "pu", "  ")))
    shards.append(new Shard(List("nn", "ic", "ti", "e ", "us", "ap", "Co", "ar")))
    shards.append(new Shard(List("ou", " t", "ng", "ki", " s", "it", "ic", "  ")))
    shards.append(new Shard(List(" S", ", ", "il", " c", "ul", "he", " o", "s ")))
    shards.append(new Shard(List("on", "  ", "d ", "t ", "le", "ti", "he", "  ")))
    shards.append(new Shard(List("nd", "he", "ua", "ng", "ol", "le", "at", "  ")))
    shards.append(new Shard(List("on", "h ", "c ", "br", "e ", "er", "mm", ". ")))
    val dictionaryFile = new File(new File(".").getCanonicalPath + "/resources/words")
    val result         = ShredderPuzzle.getBestCandidate(shards.toList, dictionaryFile)
    println(result)
    assert(FrequencyAnalyzer.dropSpaces(result.shard.toString()) == FrequencyAnalyzer.dropSpaces(
      "CLAUDESHANNONFOUNDEDINFORMATIONTHEORY,WHICHISTHEBASISOFPROBABILISTICLANGUAGEMODELSANDOFTHECODEBREAKINGMETHODSTHATYOUWOULDUSETOSOLVETHISPROBLEM,WITHTHEPAPERTITLEDAMATHEMATICALTHEORYOFCOMMUNICATION,PUBLISHEDINTHISYEAR."))
  }
}
