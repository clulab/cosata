package inferenceengine.iml.states

import edu.arizona.sista.struct.Lexicon
import inferenceengine.struct.{ObjectInstance, PatternMatchInfPat, Remap, VariableValueLUT}

import scala.collection.mutable.ArrayBuffer

/**
  * Storage class for a given state/step in an inference simulation
  * Created by peter on 6/22/18.
  */
class InferenceState {
  // Name (default is unnamed)
  var name:String = InferenceState.STATE_UNNAMED

  // Objects/Agents
  val instances = new ArrayBuffer[ObjectInstance]

  // Table rows
  //val rowsGiven = new ArrayBuffer[TableRow]
  //val rowsInferred = new ArrayBuffer[TableRow]

  // Inference patterns that were run during this state
  val inferencePatternsExecuted = new ArrayBuffer[PatternMatchRunRecord]

  // Extra explanatory text provided from inference patterns/rules.
  val extraExplanatoryText = new ArrayBuffer[ExtraExplanatoryText]


  /*
   * Cloning
   */
  override def clone():InferenceState = {
    val out = new InferenceState()

    out.name = this.name

    for (inst <- this.instances) {
      out.instances.append( inst.cloneShallow(inst.name) )
    }

    for (ipe <- this.inferencePatternsExecuted) {
      inferencePatternsExecuted.append( ipe.clone() )
    }

    for (eet <- this.extraExplanatoryText) {
      extraExplanatoryText.append( eet )
    }

    // Return
    out
  }


  /*
   * Instances
   */
  def addInstance(in:ObjectInstance):Boolean = {
    if (!instanceExists(in.name)) {
      instances.append( in )
      return true
    }
    // Default return -- an ObjectInstance with that name already exists
    false
  }

  def getInstance(name:String):ObjectInstance = {
    for (i <- 0 until instances.length) {
      if (instances(i).name == name) return instances(i)
    }

    val validNames = instances.map(_.name)
    throw new RuntimeException("ERROR: getInstance(): instance with name '" + name + "' not found. (Valid names: " + validNames.mkString(", ") + ")")
  }

  def getInstanceSafe(name:String):Option[ObjectInstance] = {
    for (i <- 0 until instances.length) {
      if (instances(i).name == name) return Some(instances(i))
    }

    // Instance not found -- return None
    None
  }

  def getInstance(idx:Int):ObjectInstance = {
    instances(idx)
  }

  def getAllInstances():Array[ObjectInstance] = {
    instances.toArray
  }

  def numInstances():Int = {
    instances.length
  }

  def instanceExists(name:String):Boolean = {
    for (i <- 0 until instances.length) {
      if (instances(i).name == name) {
        return true
      }
    }
    // Default return
    false
  }

  /*
   * Name
   */
  def setName(in:String): Unit = {
    name = in
  }

  /*
   * Pattern matches that were executed
   */
  def addExecutedInferencePattern(in:PatternMatchInfPat, instRemap:Remap, wasNestedCall:Boolean = false, wasAutoCall:Boolean = false, notes:String = ""):PatternMatchRunRecord = {
    val pmr = new PatternMatchRunRecord(in, instRemap, wasNestedCall, wasAutoCall, notes)
    inferencePatternsExecuted.append(pmr)
    // Return reference to pmr
    pmr
  }


  /*
   * Explanatory Text
   */
  def addExplanatoryText(source:String, text:String) {
    extraExplanatoryText.append( new ExtraExplanatoryText(source, text) )
  }



  /*
   * Other accessors
   */
  def getStateVariables():Option[VariableValueLUT] = {
    // Step 1: Find reference to lexicon
    var lexiconRef:Option[Lexicon[String]] = None
    for (infPat <- inferencePatternsExecuted) {
      val infPatVariables = infPat.patternMatch.variableValueLUT
      lexiconRef = Some(infPatVariables.lexicon)
    }

    if (lexiconRef.isEmpty) {
      return None
    }

    // Step 2: Create new VariableValueLUT to hold the variables from whichever inference patterns were run
    val out = new VariableValueLUT(lexiconRef.get)

    // Step 3: Add variables from each inference pattern to new variableLUT
    for (infPat <- inferencePatternsExecuted) {
      if ((!infPat.wasNestedCall) && (!infPat.wasAutoCall)) {
        val infPatVariables = infPat.patternMatch.variableValueLUT
        println ("Variables from inference pattern: " + infPatVariables.toString())
        out.add(infPatVariables)
      }
    }

    // Return
    Some(out)
  }



  /*
   * String methods
   */
  override def toString():String = {
    toString(includeAutoPatterns = true)
  }

  def toString(includeAutoPatterns:Boolean = true):String = {
    val os = new StringBuilder

    os.append("ObjectInstances: \n")
    for (i <- 0 until instances.size) {
      os.append(i + ": " + instances(i).toString() + "\n")
    }

    os.append("\n")
    os.append("Inference Patterns Executed: \n")
    for (i <- 0 until inferencePatternsExecuted.size) {
      if ((!inferencePatternsExecuted(i).wasAutoCall) || (includeAutoPatterns)) {
        os.append(i + ": " + inferencePatternsExecuted(i).toString() + "\n")
        os.append("\n")
      }
    }

    os.append("\n")
    os.append("Extra Explanatory Text:\n")
    for (i <- 0 until extraExplanatoryText.length) {
      os.append(i + ": " + extraExplanatoryText(i) + "\n")
    }

    // Return
    os.toString()
  }

}


object InferenceState {
  val STATE_UNNAMED   =   "unnamed"
}

// Storage class for extra explanatory text
class ExtraExplanatoryText(val source:String, val text:String) {

  /*
   * String methods
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append(source + ": " + text)

    // Return
    os.toString()
  }
}


// Storage class for a record of an inference pattern being executed
class PatternMatchRunRecord(val patternMatch:PatternMatchInfPat, var instRemap:Remap, val wasNestedCall:Boolean, val wasAutoCall:Boolean, var shortDescription:String = "", var notes:String = "") {

  /*
   * Setters
   */
  def setShortDescription(in:String) {
    shortDescription = in
  }

  def setNotes(in:String): Unit = {
    notes = in
  }

  /*
   * Getters
   */
  def name:String = patternMatch.inferencePattern.name


  /*
   * Cloning
   */
  override def clone():PatternMatchRunRecord = {
    new PatternMatchRunRecord(patternMatch, instRemap, wasNestedCall, wasAutoCall)
  }

  /*
   * String methods
   */

  override def toString():String = {
    val os = new StringBuilder

    // Header
    os.append("Inference Pattern: " + patternMatch.inferencePattern.name)
    os.append(" (" + shortDescription + ") ")

    if (notes.length > 0) {
      os.append(" (" + notes + ")")
    }

    // Boolean flag: nested call
    if (wasNestedCall) {
      os.append( "(nested call)".formatted("%20s") )
    } else {
      os.append( "".formatted("%20s") )
    }

    // Boolean flag: auto call
    if (wasAutoCall) {
      os.append( "(auto call)".formatted("%20s") )
    } else {
      os.append( "".formatted("%20s") )
    }

    os.append("\n")

    // Remapping
    os.append("instRemap: " + instRemap.toString() + "\n")

    // Body
    os.append(patternMatch.toStringMinimalSummary())

    // Return
    os.toString()
  }

  def toStringMinimalHTML():String = {
    val os = new StringBuilder

    // Header
    os.append("<b>" + this.name + ":</b> " + shortDescription)

    if (notes.length > 0) {
      os.append(" (" + notes + ")")
    }

    // Boolean flag: nested call
    if (wasNestedCall) {
      os.append( "(nested call)".formatted("%20s") )
    } else {
      os.append( "".formatted("%20s") )
    }

    // Boolean flag: auto call
    if (wasAutoCall) {
      os.append( "(auto call)".formatted("%20s") )
    } else {
      os.append( "".formatted("%20s") )
    }

    os.append("\n")

    // Return
    os.toString()
  }

}