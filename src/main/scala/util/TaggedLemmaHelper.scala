package util

import edu.arizona.sista.processors.Sentence
import edu.arizona.sista.struct.{Counter, Lexicon}

/**
 * Helper functions for working with tagged lemmas (of the form "lemma_TAG")
 * Created by peter on 2/3/16.
 */

object TaggedLemmaHelper {
  val MODE_WORDS        = 1
  val MODE_LEMMAS       = 2

  val contentTags = Array("NN", "VB", "JJ", "RB", "IN", "CD")
  //val contentTags = Array("NN", "VB", "JJ")

  // Make tagged lemma string format
  def mkTLemma(lemma:String, tag:String):String = {
    lemma.trim().toLowerCase() + "_" + mkGroupedTag(tag)
  }

  // Group tags -- all nouns to NN, all verbs to VB, etc.
  def mkGroupedTag(tag:String):String = {
    var groupedTag:String = tag
    if (groupedTag.length > 2) groupedTag = groupedTag.substring(0, 2)
    groupedTag.toUpperCase
  }

  // Check if a string is already likely a tagged lemma (e.g. lemma_TAG, cat_NN)
  // Note: Currently a simple test
  def isTLemmaFormat(text:String):Boolean = {
    if (!text.contains("_")) return false
    true
  }

  def hasContentTag(queryTag:String):Boolean = {
    for (contentTag <- contentTags) {
      if (queryTag.startsWith(contentTag)) {
        return true
      }
    }
    false
  }


  def generateTLemmas(sent:Sentence, lexicon:Lexicon[String], stopLemmas:Array[String]):(Array[Int], Array[Int]) = {
    val tWords = Array.fill[Int](sent.words.length)(-1)
    val tLemmas = Array.fill[Int](sent.words.length)(-1)

    for (i <- 0 until sent.words.length) {
      val word = sent.words(i).toLowerCase
      val lemma = sent.lemmas.get(i).toLowerCase
      val tag = sent.tags.get(i)

      if (TaggedLemmaHelper.hasContentTag(tag) && (!stopLemmas.contains(lemma.toLowerCase))) {
        // Filter stopwords/non-content lemmas
        // Make tagged words/lemmas

        val tWord = TaggedLemmaHelper.mkTLemma(word, tag)
        val tWordIdx = lexicon.add(tWord)       // Lexicon IDs (hashmap, for faster processing)
        tWords(i) = tWordIdx

        val tLemma = TaggedLemmaHelper.mkTLemma(lemma, tag)
        val tLemmaIdx = lexicon.add(tLemma)     // Lexicon IDs (hashmap, for faster processing)
        tLemmas(i) = tLemmaIdx

      }
    }

    // Return
    (tWords, tLemmas)
  }


  // Generates a counter with the tLemma lexicon ID as the key, and the number of occurrences as the value.
  // Accepts multiple sentences
  def generateTLemmaCounterSents(sentences:Array[Sentence], lexicon:Lexicon[String], stopLemmas:Array[String] = Array.empty[String]):Counter[Int] = {
    var out = new Counter[Int]

    for (sent <- sentences) {
      out = out + generateTLemmaCounterSent(sent, lexicon, stopLemmas)
    }

    // Return
    out
  }

  // Generates a counter with the tLemma lexicon ID as the key, and the number of occurrences as the value.
  // Accepts single sentence
  def generateTLemmaCounterSent(sent:Sentence, lexicon:Lexicon[String], stopLemmas:Array[String] = Array.empty[String]):Counter[Int] = {
    val out = new Counter[Int]

    // Step 1: Generate tagged Lemmas
    val (tWords, tLemmas) = generateTLemmas(sent, lexicon, stopLemmas)

    // Step 2: Using ONLY LEMMAS, generate counter
    for (tLemma <- tLemmas) {
      if (tLemma >= 0) {                // Words filtered by generateTLemmas() have a value of -1.
        out.incrementCount(tLemma)
      }
    }

    // Return
    out
  }


  // This function takes a counter as input, and returns another counter with binary values (i.e. 1 for all keys with >= 1.0 as their value)
  def mkBinaryCounter(in:Counter[Int]):Counter[Int] = {
    val out = new Counter[Int]

    for (key <- in.keySet) {
      if (in.getCount(key) >= 1.0) {
        out.setCount(key, 1.0)
      }
    }

    // Return
    out
  }


  // Convert from a counter of lexicon IDs to a counter of tagged lemma strings.
  def convertLexiconCounter(in:Counter[Int], lexicon:Lexicon[String]):Counter[String] = {
    val out = new Counter[String]

    for (key <- in.keySet) {
      val value = in.getCount(key)
      val tLemma = lexicon.get(key)
      out.setCount(tLemma, value)
    }

    // Return
    out
  }


  // Convert a counter into an IDF vector, suitable for performing dot products
  // Input: Vector of term frequencies
  // Output: Vector of unit length, with each element tf.idf weighted
  def counterToIDFVector(in:Counter[Int], docFreqCounter:Counter[Int]):Counter[Int] = {
    val out = new Counter[Int]

    // First pass -- convert to tf.idf
    for (key <- in.keySet) {
      val termFreq = in.getCount(key)
      var docFreq = docFreqCounter.getCount(key)
      if (docFreq == 0) docFreq = 1

      val tfidf = termFreq / math.log10(1 + docFreq)
      out.setCount(key, tfidf)
    }

    // Second pass -- normalize to unit length
    val length = out.l2Norm
    for (key <- out.keySet) {
      val currentValue = out.getCount(key)
      out.setCount(key, currentValue / length)
    }

    // Return vector of unit length
    out
  }


  // Normalize counter to unit length
  def normalizeCounter(in:Counter[Int]):Counter[Int] = {
    val out = new Counter[Int]

    // Second pass -- normalize to unit length
    val length = in.l2Norm
    for (key <- in.keySet) {
      val currentValue = in.getCount(key)
      out.setCount(key, currentValue / length)
    }

    // Return vector of unit length
    out
  }


  // Compare whether two arrays of lexicon IDs contain the same elements
  def compareLexiconIdxs(in1:Array[Int], in2:Array[Int]):Boolean = {
    if (in1.length != in2.length) return false
    for (idx <- in1) {
      if (!in2.contains(idx)) return false
    }
    // Return
    true
  }

  // Convert a string of lexicon IDs to a plain text string
  def getString(in:Array[Int], lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (idx <- in) {
      os.append(lexicon.get(idx) + " ")
    }

    // Return
    os.toString.trim()
  }

}
