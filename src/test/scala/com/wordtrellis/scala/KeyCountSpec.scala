
package com.wordtrellis.scala

import org.scalatest.FunSpec


/**
  * Spec for KeyCount
  *
  * @author : Todd Cook
  *
  */
class KeyCountSpec extends FunSpec {
  describe("A KeyCount object should: ") {

    it("provide access to a key object and a count value") {
      val keyCount = new KeyCount("one key", 11)
      assert(keyCount.key === "one key")
      assert(keyCount.count === 11)
    }

    it("determine equality solely based on the key's hashCode value") {

      val keyCount = new KeyCount("one key", 11)
      val keyCount2 = new KeyCount("two key", 21)
      assert(keyCount != keyCount2)
      val keyCount3 = new KeyCount("one key", 11)
      assert(keyCount == keyCount3)
    }

    it("be typed according to the key's type parameter") {
      val keyCount = new KeyCount("one key", 11)
      val keyCount2 = new KeyCount(java.lang.Long.valueOf(502L), 11)
      assert(keyCount != keyCount2)
      assert(keyCount.key.getClass != keyCount2.key.getClass)
      val keyCount3 = new KeyCount("one key", 11)
      assert(keyCount.key.getClass === keyCount3.key.getClass)
    }
  }
}