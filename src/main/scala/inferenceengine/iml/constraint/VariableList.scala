package inferenceengine.iml.constraint

import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.extension.Tuples

import scala.collection.mutable.ArrayBuffer


/*
 * Storage class for a list of variables and their associated indicies.
 * It's assumed that the list of variables is short (<10-20), so an array is used instead of a map.
 */
class VariableList {
  private val variableIdx = new ArrayBuffer[String]

  /*
   * Variable Index LUT
   */
  def addVariable(name: String): Int = {
    //println ("Adding " + name)
    // Step 1: Search for existing variable with same name
    val idx = variableIdx.indexOf(name)
    if (idx >= 0) return idx
    // Step 2: If not found, add variable
    variableIdx.append(name)
    return (variableIdx.size - 1)
  }

  def getVariableIdx(name: String): Int = {
    val idx = variableIdx.indexOf(name)
    return idx
  }

  def getVariableAtIdx(idx: Int): String = {
    if (idx >= variableIdx.size) return ""
    variableIdx(idx)
  }

  def size:Int = variableIdx.size

  def getAllVariableNames():Array[String] = {
    variableIdx.toArray
  }

}

