package inferenceengine.util

import edu.arizona.sista.struct.Lexicon

import scala.collection.mutable

/**
  * The CoreNLP lemmatizer is sometimes inconsistent in the way different words (e.g. heating/cooling) are lemmatized in the same sentences.
  * This allows manually substituting all occurrances of tagged lemma X (e.g. heating_VB) with expected tagged lemma Y (e.g. heat_VB).
  * Created by user on 7/19/18.
  */

object LemmatizerSubstitutions {
  val findReplaceLUT = mutable.Map[Int, Int]().withDefaultValue(-1)

  def load(filename:String, lexicon:Lexicon[String]): Unit = {
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      //println (line)
      // Parse find/replace lines from file
      val split = line.split("\t")
      val find = split(0).trim()      // e.g. heating_VB
      val replace = split(1).trim()   // e.g. heat_VB

      // Convert find/replace to lexicon indices
      val findIdx = lexicon.add( find )
      val replaceIdx = lexicon.add ( replace )

      // Store in look-up table
      findReplaceLUT(findIdx) = replaceIdx
    }

    println ("* LemmatizerSubstitutions: loaded " + findReplaceLUT.size + " substitutions.")
    println ( findReplaceLUT )
  }

  def lookupReplacement(idx:Int):Int = {
    val replacementIdx = findReplaceLUT(idx)
    // If not found, there is no replacement -- return the original lexicon index
    if (replacementIdx == -1) return idx

    // If found, return the replacement index
    replacementIdx
  }

}
