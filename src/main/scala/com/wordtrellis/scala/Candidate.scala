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

import math.Ordered
import scala.collection.mutable.HashMap
import scala.collection.immutable.List

/**
 *
 * @author : Todd Cook
 * @since : Feb 20, 2010 11:28:08 AM
 */
class Candidate(val cipherText: List[String],
                val decipheredText: List[String],
                var charMap: HashMap[java.lang.Character, java.lang.Character],
                val useHints: Boolean,
                var formula: String)
        extends Ordered[Candidate]
{
    def this(cipherText: String, decipheredText: String)  {
        this ( List(cipherText), List(decipheredText),
               new HashMap[java.lang.Character, java.lang.Character](), false, "")
    }

    def this(cipherText: List[String], decipheredText :List[String])  {
        this (cipherText, decipheredText,
              new HashMap[java.lang.Character, java.lang.Character](), false, "")
    }

    def this(cipherText: String, decipheredText: List[String], charMap: HashMap[java.lang.Character, java.lang.Character])  {
        this (List(cipherText), decipheredText, charMap, false, "")
    }

    def this (cipherTextList :List[String] , decipheredText: List[String], charMap: HashMap[java.lang.Character, java.lang.Character])   {
        this ( cipherTextList  , decipheredText, charMap, false, "")
    }

  def this(){ this( List[String](), List[String]() )}
  

    var score = 0;

    var possibleHints = new HashMap[java.lang.Character, java.lang.Character]()

    // todo replace this in calling code with the VAL reference
    def getDecipheredText() = decipheredText// .mkString(" ")

    def getDecipheredTextDisplay() = decipheredText.mkString(" ")
    
    def getCipherTextDisplay() = cipherText.mkString(" ")

    def getPossibleHints() = possibleHints

    def addPossibleHints(hints: HashMap[java.lang.Character, java.lang.Character]) {
        hints.keysIterator.toList.foreach(k => possibleHints.put(k, hints.get(k).get))
    }

    override def toString(): String = {
        "Candidate{" +
                "cipherText='" + cipherText.mkString(" ") + '\'' +
                ", decipheredText='" + decipheredText.mkString(" ") + '\'' +
                ", formula='" + formula + '\'' +
                ", score=" + score + '}'
    }

    // Note: compare, equals and hashCode should all be similar in there tests
    def compare(that: Candidate) = {
        if (score > that.score) 1;
        else if (score < that.score) -1;
        else 0;
    }

    override def equals(other: Any) = other match {
        case that: Candidate => this.decipheredText == that.decipheredText
        case _ => false
    }

    override def hashCode() = decipheredText.hashCode

    def getCharMap() = charMap
}
