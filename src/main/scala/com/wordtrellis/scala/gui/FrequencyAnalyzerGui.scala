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
import com.wordtrellis.scala.{Vingenere, FrequencyAnalyzer}

/**
 * Demo class for Frequency-Analyzer
 *
 * @author Todd Cook
 * @since 4/16/11
 */

object FrequencyAnalyzerGui extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Frequency Analyzer GUI : Vingenere Cipher"

    val plainTextLabel =new Label("Plain Text:")
    val plainText = new TextArea("Plain Text", 10, 55) {
      text = ""
    }
    val plainTextScrollPane = new ScrollPane(plainText) {
      preferredSize = new java.awt.Dimension(600, 300)
    }

    val encipher = new Button {
      text = "Encipher";
    }
    val decipher = new Button {
      text = "Decipher";
    }
    val lblKey = new Label {
      text = "Key: "
    }
    val txtKey = new TextField {
      text = ""
      preferredSize=  new Dimension(100,20)
    }

    val cipherTextLabel =  new Label("Cipher Text")
    val cipherText = new TextArea("Cipher Text", 10, 55) {
      text = ""
    }
    val cipherTextScrollPane = new ScrollPane(cipherText) {
      preferredSize = new java.awt.Dimension(600, 300)
    }

    val tablePane = new MigPanel("wrap 4") {
      add (plainTextLabel)
      add(plainTextScrollPane, "span")
      add(encipher)
      add(decipher, "span")
      add(lblKey)
      add(txtKey, "span")
      add(cipherTextLabel)
      add(cipherTextScrollPane)

    }

    //GUI Contents
    contents = new BoxPanel(Orientation.Vertical) {
      contents += tablePane
      border = Swing.EmptyBorder(15, 15, 5, 15)
    }

    listenTo(encipher)
    listenTo(decipher)

    val vg = new Vingenere(Vingenere.UPPER_ENGLISH)

    reactions += {
      case ButtonClicked(`encipher`) =>
        cipherText.text = vg.encipher(
          FrequencyAnalyzer.dropNonLettersForceUpperPreserveSpaces(plainText.text).replaceAll("\\s",""),
          txtKey.text)

    }
    reactions += {
      case ButtonClicked(`decipher`) =>
        plainText.text = vg.decipher(
          FrequencyAnalyzer.dropNonLettersForceUpperPreserveSpaces(cipherText.text).replaceAll("\\s",""), txtKey.text)
    }
  }
}