package inferenceengine.struct

import edu.arizona.sista.struct.Lexicon
import util.LexiconUtil

import scala.collection.mutable

/**
  * Created by user on 7/27/18.
  */
class VariableValueLUT(val lexicon:Lexicon[String]) {
  val LUT = mutable.Map[String, OmniValue]()

  def exists(name:String):Boolean = {
    if (LUT.contains(name)) return true
    false
  }

  def setVariable(name:String, value:OmniValue) {
    LUT(name) = value
  }

  def setVariable(name:String, value:Array[Int]) {
    LUT(name) = OmniValue.mkFrom1DArray(value)
  }

  def setVariable(name:String, value:String): Unit = {
    setVariable(name, OmniValue.mkFrom1DArray( LexiconUtil.strToLexiconIdxs(value, lexicon)) )
  }

  def getVariable(name:String):OmniValue = {
    LUT(name)
  }

  def getVariableStr(name:String):String = {
    getVariable(name).toStringValueOnly(lexicon)
  }

  def getVariableStrSafe(name:String):(String, Boolean) = {
    if (exists(name)) {
      return (getVariableStr(name), true)
    } else {
      return ("", false)
    }
  }

  def getVariableDouble(name:String):Option[Double] = {
    val (value, success) = getVariableStrSafe(name)
    if ((value.length == 0) || (success == false)) return None

    val str = value
    try {
      Some( str.toDouble )
    } catch {
      case e:NumberFormatException => None
    }
  }

  def getVariableDoubleSafe(name:String):(Double, Boolean) = {
    if (exists(name)) {
      val value = getVariableDouble(name)
      if (value.isEmpty) return (0.0, false)
      return (value.get, true)
    } else {
      return (0.0, false)
    }
  }


  def getSortedVariableNames():Array[String] = {
    LUT.keySet.toArray.sorted
  }

  def size:Int = {
    LUT.size
  }

  // Return the number of populated variables
  def numPopulatedVariables():Int = {
    var sum:Int = 0
    for (varName <- LUT.keySet) {
      if (LUT(varName).length > 0) sum += 1
    }
    // Return
    sum
  }


  /*
   * Adding VariableValueLUTs
   */
  def addWithPrefix(prefix:String, in:VariableValueLUT) {
    for (key <- in.LUT.keySet) {
      this.setVariable(prefix + key, in.getVariable(key))
    }
  }

  def add(in:VariableValueLUT): Unit = {
    addWithPrefix("", in)
  }

  /*
   * Comparison
   */

  def isIdenticalTo(in:VariableValueLUT):Boolean = {
    //## println ("isIdenticalTo: started... ")

    // Fast check: Check to see if there are a different number of variables stored in each LUT
    if (this.LUT.size != in.LUT.size) {
      //## println ("\tsizes are different")
      return false
    }

    // Check to see if the variable values are equal
    for (key <- this.LUT.keys) {
      if (!in.LUT.contains(key)) {
        //## println ("\tin is missing: " + key)
        return false
      }
      if (!in.LUT(key).compare( this.LUT(key) )) {
        //## println ("\tValues are different on key (" + key + "): " + in.LUT(key).toString(lexicon) + " / " + this.LUT(key).toString(lexicon))
        return false
      }
    }

    //## println ("\tLUTs are identical")
    // Default return -- if we reach here, the LUTs are identical
    true
  }

  /*
   * Cloning
   */
  override def clone():VariableValueLUT = {
    val out = new VariableValueLUT(lexicon)

    for (key <- LUT.keySet) {
      out.setVariable(key, getVariable(key).clone())
    }

    // Return
    out
  }

  /*
   * String
   */
  override def toString():String = {
    // Return
    toStringDelim(delim = ", ")
  }

  def toStringDelim(delim:String = ", "):String = {
    val os = new StringBuilder

    val keys = getSortedVariableNames()
    for (i <- 0 until keys.length) {
      val key = keys(i)
      os.append(key + " -> " + getVariableStr(key) )
      if (i < keys.length-1) os.append(delim)
    }

    // Return
    os.toString()
  }

}
