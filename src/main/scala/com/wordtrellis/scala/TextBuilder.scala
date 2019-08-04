
package com.wordtrellis.scala

/**
  * Helper class / Builder pattern for morphing text
  *
  * @author Todd Cook
  *
  */

class TextBuilder(val originalText: String) {

  private var morphedText = originalText

  def text(): String = morphedText

  def dropSpaces(): TextBuilder = {
    morphedText = FrequencyAnalyzer.dropSpaces(morphedText)
    this
  }

  def forceUpper(): TextBuilder = {
    morphedText = FrequencyAnalyzer.forceUpper(morphedText)
    this
  }

  def dropNonLettersForceUpperPreserveSpaces(): TextBuilder = {
    morphedText = FrequencyAnalyzer.dropNonLettersForceUpperPreserveSpaces(morphedText)
    this
  }

  def dropNonLettersForceUpper(): TextBuilder = {
    morphedText = FrequencyAnalyzer.dropNonLettersForceUpper(morphedText)
    this
  }

  def dropNonLetters(): TextBuilder = {
    morphedText = FrequencyAnalyzer.dropNonLetters(morphedText)
    this
  }

}