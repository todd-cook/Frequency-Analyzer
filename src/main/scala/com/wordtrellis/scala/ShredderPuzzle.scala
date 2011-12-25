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

import collection.mutable.ListBuffer
import java.io.File

/**
 * A collection of objects and methods for solving shredder puzzles
 * @author Todd Cook
 * @since 12/14/11 12:30 PM
 */
class Shard(val rows: List[String]) {
  var index = 0

  def setIndex(newIndex: Int) {
    index = newIndex
  }

  override def toString() = rows.mkString("")
}

/**
 * Shards may be merged together to form a larger shard, the indices joined are preserved
 * so as to limit unnecessary combinatorial growth
 */
class ShardCandidate(val shard: Shard, val indices: List[Int]) {
  var score = 0;

  override def toString() = "Score: " + score + ", indices: " +
    indices.mkString(",") + "; text: " + shard.toString()
}

object ShredderPuzzle {

  /**
   * Merge two shards together
   */
  def merge(shard1: Shard, shard2: Shard): Shard = {
    new Shard((shard1.rows zip shard2.rows).map(a => a._1 + a._2))
  }

  /**
   * Generate combinations of ShardCandidates, if they don't already exist;
   * each shard is paired front & back
   */
  def generate(shards: List[Shard], candidates: List[ShardCandidate]): List[ShardCandidate] = {
    val shardBuilder = new ListBuffer[ShardCandidate]()
    candidates.foreach(c => {
      shards.foreach(s => {
        var index = shards.indexOf(s)
        if (!c.indices.contains(index)) {
          var shard2 = merge(c.shard, s)
          var shard3 = merge(s, c.shard)
          shardBuilder.append(new ShardCandidate(shard2, c.indices ::: List(index)))
          shardBuilder.append(new ShardCandidate(shard3, c.indices ::: List(index)))
        }
      })
    })
    shardBuilder.toList
  }

  /**
   * Progressively merge shards together, score them by legibility, sort and
   * take the top percentage so that the list of possible candidates doesn't grow too large
   * @return the top ShardCandidate - a single amalgamation of all the shards
   */
  def getBestCandidate (shards :List[Shard], dictionary :File) :ShardCandidate = {
      getCandidates(shards, dictionary).last
  }

  /**
   * Progressively merge shards together, score them by legibility, sort and
   * take the top percentage so that the list of possible candidates doesn't grow too large
   * @return list of candidates
   */
    def getCandidates (shards :List[Shard], dictionary :File) :List[ShardCandidate] = {
    // heuristic figure to limit the combinatory dataset used to harvest results
    // note: for this problem, you can limit it to: shards.size, however, I wouldn't
    // recommend it for other puzzles
    val MIN_COMBOS = shards.size * 4
    val upperShards = shards.map(a => new Shard(a.rows.map(_.toUpperCase)))
    val legibilityGauge = new LegibilityGauge()
    // Loading the dictionary into the legibility gauge; accuracy skyrockets
    legibilityGauge.loadDictionary(dictionary)
    //println("dictionary loaded")
    val shardBuilder = new ListBuffer[ShardCandidate]()
    Iterator.range(0, upperShards.length).foreach(i => shardBuilder.append(
      new ShardCandidate(upperShards(i), List(i))))
    var shardCandidates = shardBuilder.toList
    while (shardCandidates(0).indices.size < upperShards.size) {
      val tmps = ShredderPuzzle.generate(upperShards, shardCandidates)
      tmps.foreach(a => {
        a.score = legibilityGauge.scoreText(a.shard.rows.mkString(" "))
      })
      val newTmps = tmps.sortBy(_.score)
      if (newTmps.length > MIN_COMBOS) {
        shardCandidates = newTmps.slice(newTmps.length - MIN_COMBOS, newTmps.length)
      } else {
        shardCandidates = newTmps
      }
    }
    //shardCandidates.foreach(println(_))
    shardCandidates
  }

  /**
   * flatten list of strings; the components of shards
   */
  def flattenMatrixLists(lists: List[List[String]]) = {
     val buf = new ListBuffer[String]();
     (0 until lists(0).length).foreach(i => {
       (0 until lists.length).foreach(j => {
         buf.append(lists(j)(i))
       })
     })
     buf.toList
   }
}

/**
 * It would be more efficient to create and evaluate lists of the indices;
 * however this isn't yet implemented
 *
 */
class ShredderPuzzle() {

  val pieces = new ListBuffer[Shard]()

  def append(shard: Shard) = pieces.append(shard)

  override def toString() = pieces.sortBy(_.index).toString()

  def setIndices(indices: List[Int]) = {
    (0 until indices.length).foreach(x => pieces(x).index = indices(x))
  }

  def setIndex(i: Int, j: Int) = {
    pieces(i).index = j
  }

  def getLists() = {
    val col = for (shard <- pieces.toList.sortBy(_.index)) yield {
      (shard.rows)
    }
    col.toList
  }
}
