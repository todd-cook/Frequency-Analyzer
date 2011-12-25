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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import collection.mutable.ListBuffer
import java.io.File

/**
 * Demonstration of automatically solving a complicated shredder puzzle
 * @author Todd Cook
 * @since 4/19/11 9:11 PM
 */

class ShredderSolverTest extends AssertionsForJUnit {

  @Test
  def testShredderSolver() {
    /**
     * This puzzle is from a popular AI class;
     * the hardest part of solving a shredder puzzle is how to represent the letters as a list of
     * strings. If spaces are given, it's easy.
     */
    var shards = new ListBuffer[Shard]()
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
    val dictionaryFile = new File(new File(".").getCanonicalPath() + "/resources/words")
    val result = ShredderPuzzle.getBestCandidate( shards.toList, dictionaryFile )
    println (result)
    assertTrue (FrequencyAnalyzer.dropSpaces(result.shard.toString()).equals(FrequencyAnalyzer.dropSpaces(
    "CLAUDESHANNONFOUNDEDINFORMATIONTHEORY,WHICHISTHEBASISOFPROBABILISTICLANGUAGEMODELSANDOFTHECODEBREAKINGMETHODSTHATYOUWOULDUSETOSOLVETHISPROBLEM,WITHTHEPAPERTITLEDAMATHEMATICALTHEORYOFCOMMUNICATION,PUBLISHEDINTHISYEAR.")))
  }
}