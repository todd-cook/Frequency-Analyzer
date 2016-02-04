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

import org.scalatest.FunSpec


/**
 * Spec for KeyCount
 * @author : Todd Cook
 * @since : Mar 6, 2010 5:58:04 PM
 */
class KeyCountSpec extends FunSpec
{
    describe("A KeyCount object should: ") {

        it ("provide access to a key object and a count value") {
            var keyCount = new KeyCount("one key", 11)
            assert(keyCount.key === "one key")
            assert(keyCount.count === 11)
        }

        it ("determine equality solely based on the key's hashCode value") {

            var keyCount = new KeyCount("one key", 11)
            var keyCount2 = new KeyCount("two key", 21)
            assert(keyCount != keyCount2)
            var keyCount3 = new KeyCount("one key", 11)
            assert(keyCount == keyCount3)
       }

        it ("be typed according to the key's type parameter") {
            var keyCount = new KeyCount("one key", 11)
            var keyCount2 = new KeyCount(java.lang.Long.valueOf(502L) , 11)
            assert(keyCount != keyCount2)
            assert(keyCount.key.getClass !=  keyCount2.key.getClass )
            var keyCount3 = new KeyCount("one key", 11)
            assert(keyCount.key.getClass ===  keyCount3.key.getClass )
         }
    }
}