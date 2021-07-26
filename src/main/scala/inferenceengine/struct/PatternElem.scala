package inferenceengine.struct
import PatternElem._
import edu.arizona.sista.struct.Lexicon
import explanationgraph.TableStore

import scala.collection.mutable.ArrayBuffer

/**
  * Created by user on 6/28/18.
  */

// If patternType == TYPE_VARIABLE, then argument is the variable name.
// If patternType == TYPE_LEXICAL, then argument is the lexical pattern to match
class PatternElem(val patternType:Int, variableName:String, lexicalArguments:Array[Int], lexicon:Option[Lexicon[String]], val isRelaxable:Boolean = false, val isOptional:Boolean = false) {

  /*
   * Variable
   */

  def isVariable:Boolean = {
    if (patternType == TYPE_VARIABLE) return true
    false
  }

  def getVariableName:String = {
    if (isVariable) return variableName
    // Default
    ""
  }


  /*
   * Lexical pattern
   */
  def isLexicalPattern:Boolean = {
    if (patternType == TYPE_LEXICAL) return true
    false
  }

  def getLexicalPattern:Array[Int] = {
    if (isLexicalPattern) return lexicalArguments
    // Default
    Array.empty[Int]
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    if (isVariable) os.append("VAR:" + getVariableName)
    if (isLexicalPattern) {
      os.append("LEX:" + "(" + getLexicalPattern.mkString(", ") + ") ")
      for (lexIdx <- getLexicalPattern) {
        os.append( lexicon.get.get(lexIdx) + " " )
      }
    }

    os.append(", isRelaxable: " + isRelaxable)
    os.append(", isOptional: " + isOptional)

    os.toString
  }



}


object PatternElem {
  val TYPE_VARIABLE     = 1
  val TYPE_LEXICAL      = 2


  // Generators
  def mkVariable(variableName:String, isOptional:Boolean, isRelaxable:Boolean):PatternElem = {
    new PatternElem(TYPE_VARIABLE, variableName, Array.empty[Int], None, isRelaxable, isOptional)
  }

  // Note: assumes pattern is space delimited
  def mkLexicalPattern(pattern:String, lexicon:Lexicon[String], isOptional:Boolean, isRelaxable:Boolean):PatternElem = {
    val split = pattern.split(" ")
    val lexPattern = new Array[Int](split.size)

    for (i <- 0 until split.length) {
      lexPattern(i) = lexicon.add( split(i).toLowerCase)
    }

    // Create
    new PatternElem(TYPE_LEXICAL, "", lexPattern, Some(lexicon), isRelaxable, isOptional)
  }

  //TODO: mkLexicalPattern that performs annotation?

}
