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

import java.util.Date
import util.Random
import scala.collection.mutable.HashMap
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

/**
 *
 * @author : Todd Cook
 * @since : Mar 6, 2010 2:56:55 PM
 */
object SubstitutionBuilder {
    val ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    val ALPHABETS2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ";

    def getSubstitutionAlphabet(iOffset: Int): HashMap[Char, Char] = {
        var substitutionMap = new HashMap[Char, Char]()
        (0 to ALPHABET.length - 1).foreach(x =>
                substitutionMap.put(ALPHABET.charAt(x), ALPHABETS2.charAt(x + iOffset)))
        substitutionMap
    }

    /**
     * Create a random substitution alphabet, prepoulating with hints
     */
    def getRandomSubstitutionAlphabet(hints: HashMap[Char, Char])
    : HashMap[Char, Char] = {
        var random = new Random(new Date().getTime());
        var substitutionMap = new HashMap[Char, Char]()
        var availableKeys = ALPHABET.toCharArray.toList
        var availableValues = ALPHABET.toCharArray.toList
        var key: Char = ' ';
        var substitutionLetter: Char = ' ';

        if (hints != null) {
            hints.keysIterator.toList.foreach(key => substitutionMap.put(key, hints.get(key).get))
            availableKeys = availableKeys diff hints.keySet.toList
            availableValues = availableValues diff hints.values.toList
        }

        (0 to availableKeys.length - 1 ).toList.reverse.foreach(ii => {
//            println("ii: "+ii)
//            println("availableKeys : "+  availableKeys.mkString(", ") )
//            println("availableValues : "+ availableValues.mkString(", ") )
            key = availableKeys(ii)
            substitutionLetter = availableValues(random.nextInt(availableKeys.length))
            substitutionMap.put(key, substitutionLetter)
            availableKeys = availableKeys filterNot ( x => x.asInstanceOf[Char] == key)
            availableValues = availableValues filterNot (y => y.asInstanceOf[Char] == substitutionLetter)
        })
        substitutionMap
    }

    //TODO
    // private def sortListLike ( listToSort, listParadigm)
    // e.g.   listToSort random character mappings left,  listParadigm - target corpus frequency map 

    /**
     * used for getting creating substitution alphabets from the mapping of a decimated alphabet
     */
    def getSubstitutionAlphabet(iOffset: Int, hm: HashMap[Char, Char]) : HashMap[Char, Char] =
        {
            var substitutionMap = new HashMap[Char, Char]()
            var key:Char = ' '; // key
            var key2:Char = ' '; // decimated value
            var substitutionLetter:Char = ' ';

            (0 to ALPHABET.length).foreach(ii => {
                key = ALPHABET.charAt(ii)
                key2 = hm.get(key).get
                var iDecimatedStart = ALPHABET.indexOf(key2);
                substitutionLetter = ALPHABETS2.charAt(iDecimatedStart + iOffset)
                substitutionMap.put(key, substitutionLetter)
            })
            substitutionMap
        }

    /**
     * Plain text should be converted to upper case before calling
     */
    def encipherSubstituted(plainText: String, substitutionMap: HashMap[Char, Char]) : String = {
        var result = new StringBuffer()
        plainText.toCharArray.toList.foreach(c => {
            var lookup = substitutionMap.get(c);
            if (lookup != None) result.append(lookup.get)
            else result.append(c)
        })
        result.toString
    }

    /**
     * We can handle list of ciphers as well
     */
    def encipherSubstitutedList(plainTextList: List[String], substitutionMap: HashMap[Char, Char])
    : List[String] = {
        var resultList = new ListBuffer[String]()
        plainTextList.foreach(plainText =>
                resultList.append(encipherSubstituted(plainText, substitutionMap)))
        resultList.toList
    }
}