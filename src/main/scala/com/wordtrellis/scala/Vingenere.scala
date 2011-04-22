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

/**
 * Utility class for Vingenere Cipher
 *
 *
 *
 * @author Todd Cook
 * @since 4/19/11 9:10 PM
 */

object Vingenere {
  val UPPER_ENGLISH = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  val LOWER_ENGLISH = "abcdefghijklmnopqrstuvwxyz";

    /**
     * Most often the key length will be the first or the first couple matches
     */
 def guessKeyLength (text: String) :List[Tuple2[Int, Int]] = {
        var myRange = (2 to 26).toList
        var tupleList = for (mySeed <- myRange)yield (shiftCount(text,mySeed) )
        tupleList.sortWith( _._2  > _._2)
    }

    /**
    * Each time the key repeats, the same character transformation occurs;
    * If the text has the same letter appearing at the same distance as the key
    * then there is a match.
    */
    def shiftCount (text:String, shift:Int) :Tuple2[Int, Int] = {
        var result = 0;
        // shift the text off the back onto the front, so no matches are lost
        var shiftedText = shiftText(text, shift)
        var ii = 0;
        text.toList.foreach (letter => {
            if (letter == shiftedText(ii)){
                result = result + 1;
            }
            ii = ii + 1; })
    (shift, result)
    }

    def shiftText (text:String, shift:Int) = text.substring (text.length - shift) +
                                                      text.substring(0, text.length - shift)

    def extractLetters (text:String, shift:Int) :String ={
      if (shift == 0) return text
      var buf = new StringBuilder()
       // shift the text off the back onto the front, so no matches are lost
        var ii = shift - 1 ;
        val textChars = text.toList
        while(ii < text.length){
            buf.append(textChars(ii))
        ii = ii + shift
        }
        buf.toString
    }
}

class Vingenere (val charset: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ") {
  val CHARS = charset.toList

  def encipher (text: String, key: String): String = {
    convert(text, key.toList.map(CHARS.indexOf(_)));
  }

  def decipher (text: String, key: String) = {
    convert(text, key.toList.map(CHARS.indexOf(_)).map((26 - _)))
  }

  private def convert (text: String, key: List[Int]): String = {
    var PLAINTEXT = text.toList.map(CHARS.indexOf(_));
    var ii = 0;
    var buf = new StringBuilder();
    PLAINTEXT.foreach(letter => {
      buf.append(CHARS((key(ii % key.length) + letter) % 26))
      ii = ii + 1
    });
    buf.toString
  }
}

