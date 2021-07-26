package inferenceengine.iml.runtime

import edu.arizona.sista.struct.Lexicon
import inferenceengine.struct.{OmniValue, VariableValueLUT}
import util.LexiconUtil

/**
  * Created by user on 9/7/18.
  */
class ScopedVariableLUT(val parentScope:Option[ScopedVariableLUT], lexicon:Lexicon[String]) {
  val variables = new VariableValueLUT(lexicon)

  /*
   * Accessors
   */

  def exists(name:String):Boolean = {
    // Local case
    if (variables.exists(name)) {
      return true
    }
    // Recursive case where there is a valid parent scope
    if (parentScope.isDefined) {
      return parentScope.get.exists(name)
    }
    // Recursive stop case with no valid parent scope (i.e. we're at the top-most (global) scope)
    return false
  }

  def getVariable(name:String):(OmniValue, Boolean) = {
    if (variables.exists(name)) {
      return (variables.getVariable(name), true)
    } else {
      if (parentScope.isDefined) {
        return parentScope.get.getVariable(name)
      } else {
        return (new OmniValue(Array.empty[Array[Int]]), false)
      }
    }
  }

  def getVariableStr(name:String):(String, Boolean) = {
    if (variables.exists(name)) {
      return (variables.getVariableStr(name), true)
    } else {
      if (parentScope.isDefined) {
        return parentScope.get.getVariableStr(name)
      } else {
        return ("", false)
      }
    }
  }

  def getVariableDouble(name:String):(Double, Boolean) = {
    if (variables.exists(name)) {
      val value = variables.getVariableDouble(name)
      if (value.isDefined) {
        return (value.get, true)
      } else {
        return (0.0, false)
      }
    } else {
      if (parentScope.isDefined) {
        return parentScope.get.getVariableDouble(name)
      } else {
        return (0.0, false)
      }
    }
  }

  // Number of variables defined, along entire scope stack
  def size:Int = {
    if (parentScope.isDefined) {
      return variables.size + parentScope.size
    } else {
      return variables.size
    }
  }

  def setVariable(name:String, value:Array[Int]) {
    if (exists(name)) {
      // Variable exists -- traverse the scope stack until we find the correct scope where it's stored
      if (variables.exists(name)) {
        variables.setVariable(name, value)
      } else {
        if (parentScope.isDefined) {
          // Traverse up the scope stack
          parentScope.get.setVariable(name, value)
        } else {
          println ("ERROR: Variable '" + name + "' exists in stack, but could not be located.  This should never happen.")
        }
      }
    } else {
      // Variable is a new definition -- store it in the current scope
      variables.setVariable(name, value)
    }

  }

  def setVariable(name:String, value:String): Unit = {
    if (exists(name)) {
      // Variable exists -- traverse the scope stack until we find the correct scope where it's stored
      if (variables.exists(name)) {
        variables.setVariable(name, value)
      } else {
        if (parentScope.isDefined) {
          // Traverse up the scope stack
          parentScope.get.setVariable(name, value)
        } else {
          println ("ERROR: Variable '" + name + "' exists in stack, but could not be located.  This should never happen.")
        }
      }
    } else {
      // Variable is a new definition -- store it in the current scope
      variables.setVariable(name, value)
    }

  }


  /*
   * Get global scope
   */
  def getGlobalScope():ScopedVariableLUT = {
    if (parentScope.isDefined) {
      // Recurse case -- not yet at global scope
      return parentScope.get
    } else {
      // Stop case -- at global scope
      return this
    }
  }


  /*
   * String methods
   */
  override def toString:String = {
    val os = new StringBuilder

    var maxLength:Int = 20      // Min length of 20
    for (key <- variables.getSortedVariableNames()) {
      if (key.length > maxLength) maxLength = key.length
    }
    maxLength += 2

    os.append("variableName".formatted("%" + maxLength + "s") + "  " + "value" + "\n")
    os.append("------------".formatted("%" + maxLength + "s") + "  " + "-----" + "\n")
    for (key <- variables.getSortedVariableNames()) {
      //os.append(key.formatted("%20s") + "  " + variables.getVariableStr(key) + "\n")
      os.append(key.formatted("%" + maxLength + "s") + "  " + variables.getVariableStr(key) + "\n")
    }

    // Recurse through scope until at global scope
    if (parentScope.isDefined) {
      os.append("--\n")
      os.append(parentScope.get.toString())
    }

    // Return
    os.toString()
  }
}
