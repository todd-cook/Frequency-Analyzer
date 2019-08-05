package com.wordtrellis.scala

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordered

/**
  *
  *
  * @author Todd Cook
  *
  */
class KeyCount[T](val key: T, val count: Int) extends Ordered[KeyCount[T]] {
  // Note: compare, equals and hashCode should all be similar in there tests
  def compare(that: KeyCount[T]): Int = {
    if (count > that.count) 1
    else if (count < that.count) -1
    else 0
  }

  override def equals(other: Any): Boolean = other match {
    case that: KeyCount[T] => this.key == that.key
    case _                 => false
  }

  override def hashCode: Int    = key.hashCode
  override def toString: String = s"$key $count"
}

class FrequencyMap[T](val items: List[T]) {
  private var keyMap = new mutable.HashMap[T, Int]

  /**
    * Sample size is useful for limiting the size of frequency map entries by
    * effectively truncating rare entries, if the FrequencyMap is growing to
    * large.
    * Usually this value is only necessary when generating a large key space
    * e.g. random letter combos number four or greater.
    */
  private var samplingSize = 0

  def this() = this(List[T]()) // auxiliary constructor

  def setSampleFiltration(sample: Int): Unit = {
    samplingSize = sample
  }

  // convenience method; sometimes the number of occurrences is known; e.g. caching
  def add(item: T, occurrences: Int): Unit = {
    if (samplingSize != 0 && keyMap.size > samplingSize)
      trimSamples()

    if (!keyMap.contains(item))
      keyMap += (item -> occurrences)
    else
      keyMap += (item -> (keyMap(item) + occurrences))
  }

  def trimSamples(): Unit = {
    // TODO decide if filtration floor is helpful
    //   samplingSize=  sample; filtrationFloor
    val newKClist = getKeyCountList.slice(0, samplingSize)
    val toRemove  = getKeyCountList diff newKClist
    toRemove.foreach(x => removeKey(x.key))
  }

  def removeKey(key: T): Option[Int] = keyMap.remove(key)

  // returns a sorted list of KeyCounts
  def getKeyCountList(): List[KeyCount[T]] = {
    val cm     = keyMap
    val kcList = new ListBuffer[KeyCount[T]]()
    for ((key, count: Int) <- cm)
      kcList.append(new KeyCount(key, count))
    kcList.toList.sortWith(_ > _)
  }

  def addAll(items: List[T]): Unit = {
    items.foreach(x => add(x))
  }

  def add(item: T): Unit = {
    if (samplingSize != 0 && keyMap.size > samplingSize)
      trimSamples()

    if (!keyMap.contains(item))
      keyMap += (item -> 1)
    else
      keyMap += (item -> (keyMap(item) + 1))
  }

  // returns list of keys, sorted by frequency
  def getKeyList: List[T] =
    for { kc <- getKeyCountList().sortWith(_.count > _.count); k = kc.key } yield k

  // returns list of values, sorted by frequency
  def getValueList: List[Int] = for { kc <- getKeyCountList(); k = kc.count } yield k

  def getKeyListFloor(floor: Int): List[T] = for { kc <- floorList(floor); k = kc.key } yield k

  // items having floorVal count and above
  def floorList(floorVal: Int): List[KeyCount[T]] = getKeyCountList filter (_.count >= floorVal)

  def getKeyMap: mutable.HashMap[T, Int] = keyMap

  def size(): Int = keyMap.size

  def slice(start: Int, end: Int): List[KeyCount[T]] = getKeyCountList.slice(start, end)

  override def toString: String = keyMap.toString

  def toList(): List[String] = {
    val lines = new ListBuffer[String]()
    keyMap.keysIterator.toList.foreach(x => lines.append(s"$x : ${keyMap(x)}"))
    lines.toList
  }

  /**
    * Often a frequency map most useful as a map with probability distribution values;
    * so that the total sum of the map of probabilities equals one or nearly so.
    */
  def toProbabilityMap(): Map[T, Double] = {
    val total: Double  = keyMap.values.toList.sum * 1.0D
    val (keys, values) = keyMap.toList.unzip
    val newValues      = values.map(x => x / total)
    keys.zip(newValues).toMap
  }
}
