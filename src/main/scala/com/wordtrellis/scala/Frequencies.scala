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

import scala.collection.Map;

/**
 * Canned common letter frequencies
 *
 * @author todd
 * @since 4/21/11 8:34 PM
 */

object Frequencies {

  val ST_ENG_ALPHABET_DIST = Map(
    'a' -> 0.082d, 'b' -> 0.015d, 'c' -> 0.028d, 'd' -> 0.043d, 'e' -> 0.127d, 'f' -> 0.022d,
    'g' -> 0.02d, 'h' -> 0.061d, 'i' -> 0.07d, 'j' -> 0.002d, 'k' -> 0.008d, 'l' -> 0.04d,
    'm' -> 0.024d, 'n' -> 0.067d, 'o' -> 0.075d, 'p' -> 0.019d, 'q' -> 0.001d, 'r' -> 0.06d,
    's' -> 0.063d, 't' -> 0.091d, 'u' -> 0.028d, 'v' -> 0.01d, 'w' -> 0.023d, 'x' -> 0.001d,
    'y' -> 0.02d, 'z' -> 0.001d)

}