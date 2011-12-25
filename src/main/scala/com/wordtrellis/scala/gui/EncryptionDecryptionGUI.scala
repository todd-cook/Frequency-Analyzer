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

package com.wordtrellis.scala.gui

import scala.swing._
import event._
import util.MigPanel
import com.wordtrellis.scala._

/**
 * Demo class for Frequency-Analyzer:
 * Encryption/Decryption of Substitution ciphers; vigenere ciphers
 *
 * @author Todd Cook
 * @since 4/16/11
 */

object EncryptionDecryptionGUI extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Encryption Decryption GUI"

    val substitution = new RadioMenuItem("Substitution") {
      selected = true;
    }
    val vigenere = new RadioMenuItem("Vigenere")
    val menuGroup = new ButtonGroup(substitution,vigenere)

    menuBar = new MenuBar {
        contents += new Menu("Encryption Method") {
          contents ++= menuGroup.buttons
      }
      }

    val plainTextLabel = new Label("Plain Text:")

    val plainText = new TextArea("Plain Text", 10, 55) {
      text = ""
    }
    val plainTextScrollPane = new ScrollPane(plainText) {
      preferredSize = new java.awt.Dimension(600, 300)
    }

    val encipher = new Button("Encipher")

    val decipher = new Button("Decipher")

    val lblEncipherKey = new Label("Key/offset: ")

    val txtEncipherKey = new TextField {
      text = ""
      preferredSize=  new Dimension(100,20)
    }

    val encKeyHint = new Label("(Key should be numeric)")
    // ... because you can't add the same component twice
    val decKeyHint = new Label("(Key should be numeric or left blank for automatic decryption)")

    val lblDecipherKey = new Label("Key/offset: ")

    val txtDecipherKey = new TextField {
      text = ""
      preferredSize=  new Dimension(100,20)
    }

    val cipherTextLabel = new Label("Cipher Text")

    val cipherText = new TextArea("Cipher Text", 10, 55) {
      text = ""
    }

    val cipherTextScrollPane = new ScrollPane(cipherText) {
      preferredSize = new java.awt.Dimension(600, 300)
    }

    val encPane = new MigPanel("wrap 3")  {
      add(plainTextLabel)
      add(plainTextScrollPane, "wrap")
    }

    val encKeyPane = new MigPanel("wrap 4"){
      add(lblEncipherKey)
      add(txtEncipherKey)
      add(encipher)
      add(encKeyHint, "wrap")
    }

    val cipherPane = new MigPanel("wrap 4"){
      add(cipherTextLabel)
      add(cipherTextScrollPane, "wrap")
    }

    val cipherKeyPane = new MigPanel("wrap 4" ){
      add(lblDecipherKey)
      add(txtDecipherKey)
      add(decipher )
      add(decKeyHint, "wrap")
    }

    val tablePane = new MigPanel("wrap 4" ) {
      add(encPane, "wrap")
      add(encKeyPane, "wrap")
      add(cipherPane, "wrap")
      add(cipherKeyPane, "wrap")
    }

    //GUI Contents
    contents = new BoxPanel(Orientation.Vertical) {
      contents += tablePane
      border = Swing.EmptyBorder(15, 15, 5, 15)
    }

    listenTo(encipher)
    listenTo(decipher)
    listenTo(substitution)
    listenTo(vigenere)

    val vigenereBuilder = new Vigenere(Vigenere.UPPER_ENGLISH)
    val substitutionBuilder = new SubstitutionBuilder()

    reactions += {
    case ButtonClicked(`substitution`) =>
      encKeyHint.text = "(Key should be numeric)"
      decKeyHint.text = "(Key should be numeric or left blank for automatic decryption)"
    }
    reactions += {
    case ButtonClicked(`vigenere`) =>
      encKeyHint.text = "(Key should be alphabetical/word)"
      decKeyHint.text = "(Key should be alphabetical/word)"
    }

    reactions += {
      case ButtonClicked(`encipher`) =>
        if (substitution.selected && txtEncipherKey.text.trim.length >0) {
          val nonLetterList = FrequencyAnalyzer.extractNonLetterPositionList(plainText.text)
          val result = substitutionBuilder.encipher(
            new TextBuilder(plainText.text).dropNonLettersForceUpper().dropSpaces().text(),
            Integer.valueOf(txtEncipherKey.text).intValue())
          cipherText.text = FrequencyAnalyzer.insertNonLetterPositionList(result, nonLetterList)
        }
        else if (txtEncipherKey.text.trim.length() > 0){
          val nonLetterList = FrequencyAnalyzer.extractNonLetterPositionList(plainText.text)
          val result = vigenereBuilder.encipher(
            new TextBuilder(plainText.text).dropNonLettersForceUpper().dropSpaces().text(),
            FrequencyAnalyzer.forceUpper(txtEncipherKey.text))
          cipherText.text = FrequencyAnalyzer.insertNonLetterPositionList(result, nonLetterList)
        }
    }
    reactions += {
      case ButtonClicked(`decipher`) =>
        if (substitution.selected) {
          if (txtDecipherKey.text.trim.length == 0) {
            val nonLetterList = FrequencyAnalyzer.extractNonLetterPositionList(cipherText.text)
            val scs = new SubstitutionCipherSolver(
              new TextBuilder(cipherText.text).dropNonLettersForceUpper().dropSpaces().text())
            scs.compute();
            val result = scs.getBestCandidate().getDecipheredText().mkString("").trim
            // println("Best Guess: " + plainText + " = " + result)
            plainText.text = FrequencyAnalyzer.insertNonLetterPositionList(result, nonLetterList)
          }
          else {
            val nonLetterList = FrequencyAnalyzer.extractNonLetterPositionList(cipherText.text)
            val result = substitutionBuilder.decipher(
              new TextBuilder(cipherText.text).dropNonLettersForceUpper().dropSpaces().text(),
              Integer.valueOf(txtDecipherKey.text).intValue())
            plainText.text = FrequencyAnalyzer.insertNonLetterPositionList(result, nonLetterList)
          }
        }
        else {
         if (txtDecipherKey.text.length > 0){
          val nonLetterList = FrequencyAnalyzer.extractNonLetterPositionList(cipherText.text)
          val result = vigenereBuilder.decipher(
            new TextBuilder(cipherText.text).dropNonLettersForceUpper().dropSpaces().text(),
            txtDecipherKey.text.toUpperCase
          )
          plainText.text = FrequencyAnalyzer.insertNonLetterPositionList(result, nonLetterList)
        }
        }
    }
  }
}