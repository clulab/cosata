package inferenceengine.struct

import edu.arizona.sista.struct.Lexicon
import inferenceengine.util.LemmatizerSubstitutions
import util.TaggedLemmaHelper

/**
  * Storage class for the location of a pattern match in call in a tablerow
  * Created by user on 6/28/18.
  */
/*

// Depricated

class PatternMatchCell(val cellPat:CellPattern, val alternatives:Array[CellAlternative], val relaxed:Int = 0) {

  def getVariableValue(name:String, alt:Int):Option[Array[Int]] = {
    alternatives(alt).getVariableValue(name)
  }

  def getNumAlternatives:Int = {
    alternatives.length
  }

  /*
   * String methods
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("numAlternatives: " + alternatives.length + "\n")
    for (i <- 0 until alternatives.length) {
      os.append("\t\tAlt" + i + ": \t" + alternatives(i).toString())
      if (i < alternatives.length-1) os.append("\n")
    }

    // Return
    os.toString()
  }

  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append("numAlternatives: " + alternatives.length + "\n")
    for (i <- 0 until alternatives.length) {
      os.append("\t\tAlt" + i + ": \t" + alternatives(i).toString(lexicon))
      if (i < alternatives.length-1) os.append("\n")
    }

    // Return
    os.toString()
  }

}
*/

/*
// Depricated

// NOTE: cellWords can represent any word form (lemma/word, tagged/untagged), since it's a reference to the lexicon.
// Currently it's set to use tagged lemmas, since this increases matching robustness while having some disambiguation from part-of-speech.
class CellAlternative(val cellPat:CellPattern, val alternativeIdx:Int, val cellWords:Array[Int], val locations:Array[(Int, Int)]) {

  def getVariableValue(name:String):Option[Array[Int]] = {
    // Find variable index
    val idx = cellPat.getVariableNameIndex(name)

    if (idx >= 0) {
      //TODO: return cellText.substring(locations(idx)._1, locations(idx)._2)
      return Some(cellWords.slice(locations(idx)._1, locations(idx)._2))
    }

    // Default -- variable name not found
    None
  }


  override def toString():String = {
    val os = new StringBuilder

    for (i <- 0 until cellPat.elems.length) {
      os.append ("Elem " + i + ": \t")
      if (cellPat.getElem(i).isVariable) os.append( ("VAR:" + cellPat.getElem(i).getVariableName + " \t").formatted("%20s") )
      if (cellPat.getElem(i).isLexicalPattern) os.append( ("LEX" + " \t").formatted("%20s") )
      os.append( ("(" + locations(i)._1 + ", " + locations(i)._2 + ") \t").formatted("%10s") )
      os.append(cellWords.slice(locations(i)._1, locations(i)._2).mkString(", "))

      os.append("   ")
    }

    // Return
    os.toString()
  }

  // This version uses the lexicon to show plaintext words
  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (i <- 0 until cellPat.elems.length) {
      os.append ("Elem " + i + ": \t")
      if (cellPat.getElem(i).isVariable) os.append( ("VAR:" + cellPat.getElem(i).getVariableName + " \t").formatted("%20s") )
      if (cellPat.getElem(i).isLexicalPattern) os.append( ("LEX" + " \t").formatted("%20s") )
      os.append( ("(" + locations(i)._1 + ", " + locations(i)._2 + ") \t").formatted("%10s") )
      val elemWords = cellWords.slice(locations(i)._1, locations(i)._2)

      for (lexIdx <- elemWords) {
        os.append( lexicon.get(lexIdx) + " ")
      }
      os.append("(" + elemWords.mkString(", ") + ")")

      os.append("   ")
    }

    // Return
    os.toString()
  }


}


object CellAlternative {

  // Generator
  def mkCellAlternative(cellPat:CellPattern, alternativeIdx:Int, cellWords:Array[Int], locations:Array[(Int, Int)], lexicon:Lexicon[String]):CellAlternative = {
    // Return
    val cellWordsNormalized = doLemmatizerSubstitutions(cellWords)
    new CellAlternative(cellPat, alternativeIdx, cellWordsNormalized, locations)
  }

  // Sometimes the lemmatizer works inconsistently (e.g. heating is somethings lemmatized as heating_NN, heating_VB, or heat_VB).  This uses a look-up table to
  // manually normalize these issues.
  def doLemmatizerSubstitutions(in:Array[Int]):Array[Int] = {
    val out = new Array[Int](in.length)

    // For each word, check for a substitution.  (If no substitution is found, lookupReplacement returns the original tag)
    for (i <- 0 until in.length) {
      out(i) = LemmatizerSubstitutions.lookupReplacement( in(i) )
    }

    // Return
    out
  }

}

 */