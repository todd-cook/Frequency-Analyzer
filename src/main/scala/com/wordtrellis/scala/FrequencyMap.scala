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

package com.wordtrellis.scala;

import collection.immutable.List
import collection.mutable.{HashSet, ListBuffer, HashMap}
import io.{BufferedSource, Source}
import java.io.{ByteArrayInputStream}
import math.Ordered

/**
 *
 *
 * @author Todd Cook
* @since : Feb 20, 2010 11:28:08 AM
 */
class KeyCount[T](val key: T, val count: Int) extends Ordered[KeyCount[T]] {
    // Note: compare, equals and hashCode should all be similar in there tests
    def compare(that: KeyCount[T]) = {
        if (count > that.count) 1;
        else if (count < that.count) -1;
        else 0;
    }

    override def equals(other: Any) = other match {
        case that: KeyCount[T] => this.key == that.key // was count  && this.key == that.key
        case _ => false
    }

    override def hashCode: Int = key.hashCode // count
    override def toString() = key + " " + count
}
// 2.8 warning //<console>:13: warning: non variable type-argument T in type pattern KeyCount[T] is unchecked since it is eliminated by erasure  case that: KeyCount[T] => this.count == that.count // && this.key == that.key

class FrequencyMap[T](val items: List[T]) {
    def this() = this (List[T]()) // auxiliary constructor

    private var keyMap = new HashMap[T, Int];
    items map add _

    /**
     * Sample size is useful for limiting the size of frequency map entries by
     * effectively truncating rare entries, if the FrequencyMap is growing to
     * large.
     * Usually this value is only necessary when generating a large key space
     * e.g. random letter combos number four or greater.
     */
    private var samplingSize = 0

    def setSampleFiltration(sample: Int) = {samplingSize = sample; }

    def trimSamples() :Unit = {
        // TODO decide if filtration floor is helpful
        //   samplingSize=  sample; filtrationFloor
        var newKClist = getKeyCountList.slice(0, samplingSize)
        var toRemove = getKeyCountList diff newKClist
        toRemove.foreach(x => removeKey(x.key))
    }

    def add(item: T): Unit = {
        if (samplingSize != 0 && keyMap.size > samplingSize)
            trimSamples()

        if (!keyMap.contains(item))
            keyMap += (item -> 1)
        else
            keyMap += (item -> (keyMap.get(item).get + 1))
    }

    // convenience method; sometimes the number of occurrences is known; e.g. caching
    def add(item: T, occurrences: Int): Unit = {
        if (samplingSize != 0 && keyMap.size > samplingSize)
            trimSamples()

        if (!keyMap.contains(item))
            keyMap += (item -> occurrences)
        else
            keyMap += (item -> (keyMap.get(item).get + occurrences))
    }

    def addAll(items: List[T]): Unit = {items.foreach(x => add(x))}

    // returns list of keys, sorted by frequency
    def getKeyList: List[T] = for{kc <- getKeyCountList().sortWith(_.count > _.count); k = kc.key} yield (k)
    // returns list of values, sorted by frequency
    def getValueList: List[Int] = for{kc <- getKeyCountList(); k = kc.count} yield (k)

    def getKeyListFloor(floor: Int): List[T] = for{kc <- floorList(floor); k = kc.key} yield (k)

    def getKeyMap = keyMap

    def removeKey(key: T) = keyMap.remove(key)

    def size() = keyMap.size

    // returns a sorted list of KeyCounts
    def getKeyCountList(): List[KeyCount[T]] = {
        val cm = keyMap
        var kcList = new ListBuffer[KeyCount[T]]()
        for ((key, count) <- cm)
            kcList.append(new KeyCount(key, count))
        kcList.toList.sortWith(_ > _)
    }

    // items having floorVal count and above
    def floorList(floorVal: Int): List[KeyCount[T]] = getKeyCountList filter (_.count >= floorVal)

    def slice(start: Int, end: Int) = getKeyCountList.slice(start, end)

    override def toString = keyMap.toString

    def toList(): List[String] = {
        var lines = new ListBuffer[String]()
        keyMap.keysIterator.toList.foreach(x => lines.append(x + " : " + keyMap.get(x).get.asInstanceOf[Int]))
        lines.toList
    }

  /**
   * Often a frequency map most useful as a map with probability distribution values;
   * so that the total sum of the map of probabilities equals one or nearly so.
   */
    def toProbabilityMap() :Map[T,Double] ={
      val total :Double  = keyMap.values.toList.foldLeft(0)(_ + _) * 1.0D
      val (keys, values) =  keyMap.toList.unzip
      val newValues = values.map (x => x / total )
      keys.zip(newValues).toMap
    }

}