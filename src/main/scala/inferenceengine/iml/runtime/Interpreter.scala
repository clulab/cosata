package inferenceengine.iml.runtime

import java.io.PrintWriter

import explanationgraph.visualization.TableStoreHtmlExport
import explanationgraph.{TableRow, TableStore}
import inferenceengine.iml.model._
import inferenceengine.iml.visualization.IMLHtmlExport
import inferenceengine.struct.{CellPattern => _, _}
import util.{LexiconUtil, TaggedLemmaHelper}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import Interpreter._
import edu.arizona.sista.struct.Counter
import inferenceengine.iml.constraint.VariableList
import inferenceengine.iml.states.{InferenceState, PatternMatchRunRecord, StateSequence}
import inferenceengine.struct.ConstraintEval.{CONSTRAINT_MUSTHAVE, CONSTRAINT_MUSTHAVEOROMIT, CONSTRAINT_SHOULDHAVE}
import inferenceengine.struct.InferencePattern.{EXECUTIONMODE_AUTO, EXECUTIONMODE_AUTOREGEN, EXECUTIONMODE_NORMAL}
import inferenceengine.util.TaxonomicCompletion

import scala.collection.parallel.ForkJoinTaskSupport


/**
  * Created by user on 7/24/18.
  */

// TODO: Must take a scope (inference patterns, instances, etc., at a given time point)

class Interpreter(tablestore:TableStore, outputPath:String = "", outputPrefix:String = "", numThreads:Int = 16) {


  // States
  // A set of InferenceStates, that contain ObjectInstances
  var states = new StateSequence()
  // Automatically add executed patterns to the statespace?
  var autoAddToStateSpace:Boolean = true

  // A set of inference patterns
  val inferencePatterns = new ArrayBuffer[InferencePattern]
  var maxMatches:Int = 25000      // The maximum enumerations per inference pattern (capped to reduce explosive growth, user settable)

  // Variable/Instance remapping
  var remap:ArrayBuffer[Remap] = new ArrayBuffer[Remap]

  // Scoped variables
  var scopedVariableLUT = new ScopedVariableLUT(None, tablestore.lexicon)   // parentScope = 'None' defines global scope.

  // GenerateRow mode -- either rows are persistant, or temporary
  var generateRowMode:Int = GENERATEROW_MODE_DEFAULT

  // A list of inference patterns to highlight in debug output (makes development easier)
  var infPatternsToHighlight = Set[String]()

  // Last error string
  val lastErrorStr = new StringBuilder
  var lastStatement:Option[Statement] = None
  val warningStr = new StringBuilder


  /*
   * Inference state population (Initialization)
   */
  def addInferencePatterns(in:Array[InferencePattern]) = {
    for (inPat <- in) {
      if (getInferencePatternIndex(inPat.name).isEmpty) {
        // This pattern is new
        inferencePatterns.append( inPat )
      } else {
        println ("* addInferencePatterns: ERROR: Inference Pattern with name '" + inPat.name + "' already exists.  Skipping.")
      }
    }
  }

  def getInferencePatternIndex(name:String):Option[Int] = {
    for (i <- 0 until inferencePatterns.length) {
      if (inferencePatterns(i).name == name) return Some(i)
    }
    // Default return
    None
  }



  /*
   * Inference states
   */

  // Share (shallow clone) the InferenceStates of another interpreter into this interpreter
  // Intended for use by a faux interpreter to be able to read state values -- should not be used to modify state, as
  //  modifications will be permanent (and deep copying the state is slow).
  // NOTE: Should call resetStates() after using shareStates(), so that that any reference to another interpreter's states is wiped,
  //  reducing the danger of any accidental modification.
  def shareStates(otherInterpreter:Interpreter): Unit = {
    // State history
    this.states = otherInterpreter.states
  }

  def resetStates() {
    states = new StateSequence()
  }

  // Sets whether excuted patterns will automatically be added to the statespace.
  def setAutoAddToStateSpace(in:Boolean): Unit = {
    autoAddToStateSpace = in
  }


  /*
   * Remapping
   */

  def addRemapLevel(in:Remap) {
    // Merge remaps maps
    if (remap.isEmpty) {
      remap.append(in)
    } else {
      val combinedMap = remap.last.mergeHierarchy(in)
      remap.append(combinedMap)
    }

    //println ("AddRemapLevel: " + remap.last.toString() )
  }

  def removeRemapLevel() {
    if (!remap.isEmpty) {
      remap.remove( remap.length-1 )

      //if(!remap.isEmpty) println ("removeRemapLevel: current remap is now " + remap.last.toString() )
    }


  }

  // Hard reset of the remap
  def clearRemap() {
    remap.clear()
  }

  private def checkForInstPropRemap(original:InstanceProperty):InstanceProperty = {
    if (!remap.isEmpty) {
      val remappedInstName = remap.last.instances.getRemap(original.name)
      if (remappedInstName.isDefined) {
        // This instance has been remapped
        return new InstanceProperty(remappedInstName.get, original.property, original.state)
      }
    }

    // Default return
    original
  }

  private def checkForInstNameRemap(originalName:String):String = {
    if (!remap.isEmpty) {
      val remappedInstName = remap.last.instances.getRemap(originalName)
      if (remappedInstName.isDefined) {
        // This instance has been remapped
        return remappedInstName.get
      }
    }

    // Default return
    originalName
  }




  /*
   * Instance definition / assignment / accessing
   */
  def instanceDefinition(name:String, kindofExpr:Expr, variableLUT:Option[VariableValueLUT]):(Boolean, ObjectInstance) = {
    // Step 1: Evaluate 'kindOf' expression
    val (valueDouble, valueStr, success) = calculateExpr(kindofExpr, variableLUT)
    if (!success) return (false, new ObjectInstance("", Array.empty[Int], tablestore) )

    // Step 2: kindOf should always evalute to a string -- verify this
    if (valueStr.isEmpty) {
      println ("ERROR: InstanceDefinition: kindOf expression could not be evaluated to a string. (" + kindofExpr + ")")
      return (false, new ObjectInstance("", Array.empty[Int], tablestore) )
    }

    // Step 3: Convert kindof String to lexicon IDXs
    val kindofIdxs = LexiconUtil.strToLexiconIdxs(valueStr.get, tablestore.lexicon)

    // Step 4: Create new instance
    val instance = new ObjectInstance(name, kindofIdxs, tablestore)

    // Insert default properties
    instance.insertProperties(kindofIdxs)

    // Return
    (true, instance)
  }


  def instancePropertyAssignment(_instProp:InstanceProperty, expr:Expr, variableLUT:Option[VariableValueLUT]):Boolean = {
    // Handle possible remapping
    var instProp = checkForInstPropRemap(_instProp)
    /*
    println ("instancePropertyAssignment: variableLUT: " + variableLUT)
    if (variableLUT.isDefined) {
      println(variableLUT.get.toString())
    }
    */

    // Check that this instance exists
    if (states.currentState.instanceExists(instProp.name)) {
      val instance = states.currentState.getInstance(instProp.name)
      val propValue = new PropertyValue

      // Step 1: Calculate the value of the variable
      val (valueDouble, valueStr, success) = calculateExpr(expr, variableLUT)
      if (!success) return false

      // Step 2: Store it
      // First check if the numeric type is defined
      if (valueDouble.isDefined) {
        propValue.setNumeric( valueDouble.get.toString() )      // TODO: Discrete, numeric, unit
        instance.setProperty(instProp.property, propValue)
      } else {
        // Otherwise, the value is a string
        propValue.setDiscrete( valueStr.get )                   // TODO: Discrete, numeric, unit
        instance.setProperty(instProp.property, propValue)
      }

      return true   // Return Success
    } else {
      lastErrorStr.append("ERROR: Instance with name '" + instProp.name + "' could not be found.\n")
      return false  // Return Failure
    }
  }

  // Get a property of an instance, return as PropertyValue
  private def getInstanceProperty(_instProp:InstanceProperty, statePrefix:Option[StateRef]):(PropertyValue, Boolean) = {
    // Step 1: Handle possible remapping
    var instProp = checkForInstPropRemap(_instProp)

    // Step 2: Resolve reference to inference state
    // Use current state by default
    var state:InferenceState = states.currentState
    // If a state reference is given, then resolve it
    if (statePrefix.isDefined) {
      val state_ = resolveStateRef(statePrefix.get)
      if (state_.isEmpty) {
        // If empty, the state could not be resolved
        lastErrorStr.append ("ERROR: getInstanceProperty(): Failed to resolve state. \n")
        return (new PropertyValue, false)   // Return success=failure
      }

      state = state_.get
    }

    // Step 3A: Check that the instance referenced exists
    if (!state.instanceExists( instProp.name )) {
      lastErrorStr.append("ERROR: getInstanceProperty(): Instance with name '" + instProp.name + "' could not be found.\n")
      // Exit
      return (new PropertyValue, false)   // Return success=failure
    }

    // Step 3B: Get refernece to instance and it's current property value
    val instance = state.getInstance( instProp.name )
    if (!instance.propertyExists( instProp.property ) || instance.getProperty(instProp.property).isEmpty) {
      lastErrorStr.append ("ERROR: getInstanceProperty(): Property '" + instProp.property + "' of instance '" + instProp.name + "' is not populated.\n")
      // Exit
      return (new PropertyValue, false)   // Return success=failure
    }

    val propValue = instance.getProperty( instProp.property ).get

    // Return
    (propValue, true)
  }

  // Get a property of an instance, return as String
  private def getInstancePropertyStr(instProp:InstanceProperty, statePrefix:Option[StateRef]):(String, Boolean) = {
    val (propValue, success) = getInstanceProperty(instProp, statePrefix)
    if (!success) return ("", false)
    return (propValue.getValue(), true)
  }

  // Get a property of an instance, return as Double
  private def getInstancePropertyDouble(instProp:InstanceProperty, statePrefix:Option[StateRef]):(Double, Boolean) = {
    //## println ("getInstanceProperty: " + instProp.name + "   property: " + instProp.property)
    val (propValue, success) = getInstanceProperty(instProp, statePrefix)
    if (!success) {
      lastErrorStr.append("getInstanceProperty(): Could not find property (" + instProp.property + ") of instance (" + instProp.name + ").  \n")
      return (0.0, false)
    }

    // Check for successful double conversion
    val propValueDouble = propValue.getValueDouble()
    if (propValueDouble.isEmpty) {
      lastErrorStr.append("ERROR: Property '" + instProp.property + "' of instance '" + instProp.name + "' could not be converted to double.\n")
      return (0.0, false)
    }
    // Return
    return (propValueDouble.get, true)
  }

  private def getInstance(name:String, state:InferenceState = states.currentState):Option[ObjectInstance] = {
    // Handle possible remapping
    var instName = checkForInstNameRemap(name)
    if (name != instName) {
      println("NOTE: Instance name '" + name + "' remapped to '" + instName + "'.")
    }

    // Step 1: Condition
    // Step 1A: Check that the instance referenced exists
    if (!state.instanceExists( instName )) {
      lastErrorStr.append("ERROR: Instance with name '" + instName + "' could not be found.\n")
      // Exit
      return None   // Return failure
    }

    // Step 1B: Get reference to instance
    val instance = state.getInstance( instName )

    // Return
    Some(instance)
  }

  /*
   * State Reference Interpretation
   */

  // Resolve a StateRef into a specific state.
  // If None is returned, then the resolution failed.
  def resolveStateRef(ref:StateRef):Option[InferenceState] = {
    ref match {
      // State with name
      case r:StateWithName => {
        val stateMatches = states.getStatesWithName(r.name)
        if (stateMatches.length == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: Could not find state with name (" + r.name + ").  Known state names are: (" + states.getAllStateNames().toList.sorted.mkString(",") + ")\n")
          return None
        }
        if (stateMatches.length > 1) {
          lastErrorStr.append("resolveStateRef(): ERROR: State name (" + r.name + ") returned more than one match (" + stateMatches.length + ").\n")
          return None
        }
        val state = stateMatches(0)
        return Some(state)
      }

      // First state with name
      case r:StateFirstWithName => {
        val stateMatches = states.getStatesWithName(r.name)
        if (stateMatches.length == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: Could not find state with name (" + r.name + ").  Known state names are: (" + states.getAllStateNames().toList.sorted.mkString(",") + ")\n")
          return None
        }
        val state = stateMatches(0)
        return Some(state)
      }

      // Last state with name
      case r:StateLastWithName => {
        val stateMatches = states.getStatesWithName(r.name)
        if (stateMatches.length == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: Could not find state with name (" + r.name + ").  Known state names are: (" + states.getAllStateNames().toList.sorted.mkString(",") + ")\n")
          return None
        }
        val state = stateMatches.last
        return Some(state)
      }

      // State directly before the first state with name
      case r:StateBeforeName => {
        val stateMatches = states.getStateIndicesWithName(r.name)
        if (stateMatches.length == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: Could not find state with name (" + r.name + ").  Known state names are: (" + states.getAllStateNames().toList.sorted.mkString(",") + ")\n")
          return None
        }
        val stateIdx = stateMatches(0)
        if (stateIdx == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: StateBeforeName: State with name (" + r.name + ") is the first state (index 0) -- can not find a state before this.\n")
          return None
        }
        val state = states.getStateByIndex(stateIdx-1)
        return Some(state)
      }

      // State directly after the last state with name
      case r:StateAfterName => {
        val stateMatches = states.getStateIndicesWithName(r.name)
        if (stateMatches.length == 0) {
          lastErrorStr.append("resolveStateRef(): ERROR: Could not find state with name (" + r.name + ").  Known state names are: (" + states.getAllStateNames().toList.sorted.mkString(",") + ")\n")
          return None
        }
        val stateIdx = stateMatches.last
        if (stateIdx >= (states.numStates-1)) {
          lastErrorStr.append("resolveStateRef(): ERROR: StateAfterName: State with name (" + r.name + ") is the last state (index " + (states.numStates-1) + ") -- can not find a state before this.\n")
          return None
        }
        val state = states.getStateByIndex(stateIdx+1)
        return Some(state)
      }

      case _ => throw new RuntimeException("ERROR(): resolveStateRef: StateRef not recognized: " + ref.toString())
    }

    // Default
    None
  }


  /*
   * Variables, Instances (Accessors for internal use)
   */

  // Get a variable from the Variable LUT, and return as a String
  private def getVariable(name:String, variableLUT:Option[VariableValueLUT], statePrefix:Option[StateRef]):(String, Boolean) = {
    // If this refers to state variable, then it's a special case -- handle that.
    if (statePrefix.isDefined) {
      val state = resolveStateRef(statePrefix.get)
      if (state.isEmpty) return ("", false)           // If empty, the state could not be resolved

      val stateVariableLUT = state.get.getStateVariables()
      if (stateVariableLUT.isEmpty) {
        lastErrorStr.append ("ERROR: Variable '" + name + "' not found in state variable look-up table. (State variable LUT is empty)\n")
        return ("", false)
      }

      val (value, success) = stateVariableLUT.get.getVariableStrSafe(name)
      if (!success) {
        lastErrorStr.append ("ERROR: Variable '" + name + "' not found in state variable look-up table.\n")
        lastErrorStr.append ("State variable LUT: \n")
        lastErrorStr.append (stateVariableLUT.get.toString())
        return ("", false)
      }

      // Success
      return (value, true)
    }

    // First, check to see if variable exists on local variables list
    if (scopedVariableLUT.exists(name)) {
      val (value, success) = scopedVariableLUT.getVariableStr(name)
      if (success) return (value, true)
    }

    // Check that variable LUT is populated
    if (variableLUT.isEmpty) {
      lastErrorStr.append ("ERROR: Variable '" + name + "' not found in variable look-up table. (Variable look-up table is empty.)\n")
      // Exit
      return ("", false)   // Return success=failure
    }

    // Check that variable name exists
    if (!variableLUT.get.exists(name)) {
      lastErrorStr.append ("ERROR: Variable '" + name + "' not found in variable look-up table. \n")
      // Exit
      return ("", false)   // Return success=failure
    }

    // Get variable value
    val str = variableLUT.get.getVariableStr( name )

    // Return (success)
    (str, true)
  }

  // Get a variable from the Variable LUT, and return as a Double
  private def getVariableDouble(name:String, variableLUT:Option[VariableValueLUT], statePrefix:Option[StateRef]):(Double, Boolean) = {
    // If this refers to state variable, then it's a special case -- handle that.
    if (statePrefix.isDefined) {
      val state = resolveStateRef(statePrefix.get)
      if (state.isEmpty) {
        lastErrorStr.append("ERROR: State could not be resolved. \n")
        return (0.0, false)
      }           // If empty, the state could not be resolved

      val stateVariableLUT = state.get.getStateVariables()
      if (stateVariableLUT.isEmpty) {
        lastErrorStr.append ("ERROR: Variable '" + name + "' not found in state variable look-up table. (State variable LUT is empty)\n")
        return (0.0, false)
      }

      val (value, success) = stateVariableLUT.get.getVariableDoubleSafe(name)
      if (!success) {
        lastErrorStr.append ("ERROR: Variable '" + name + "' not found in state variable look-up table.\n")
        lastErrorStr.append ("State variable LUT: \n")
        lastErrorStr.append (stateVariableLUT.get.toString())
        return (0.0, false)
      }

      // Success
      return (value, true)
    }

    // First, check to see if variable exists on local variables list
    if (scopedVariableLUT.exists(name)) {
      val (value, success) = scopedVariableLUT.getVariableDouble(name)
      if (success) return (value, true)
    }

    // Check that variable LUT is populated
    if (variableLUT.isEmpty) {
      lastErrorStr.append ("ERROR: Variable '" + name + "' not found in variable look-up table. (Variable look-up table is empty.)\n")
      // Exit
      return (0.0, false)   // Return success=failure
    }

    // Check that variable name exists
    if (!variableLUT.get.exists(name)) {
      lastErrorStr.append ("ERROR: Variable '" + name + "' not found in variable look-up table. \n")
      // Exit
      return (0.0, false)   // Return success=failure
    }

    // Get variable value
    val value = variableLUT.get.getVariableDouble( name )

    // Check for successful string to double conversion
    if (value.isEmpty) {
      //## println ("ERROR: Variable '" + name + "' could not be converted to double. ")
      lastErrorStr.append ("ERROR: Value of variable '" + name + "' is empty, so it could not be converted to double.")
      // Exit
      return (0.0, false)   // Return success=failure
    }

    // Return (success)
    (value.get, true)
  }


  /*
   * Attaching/Detaching rows from instances
   */
  def attachDetachRowInstance(instanceName:String, uuidExpr:Expr, variableLUT:Option[VariableValueLUT], mode:Int):Boolean = {
    // Handle possible remapping
    val name = checkForInstNameRemap(instanceName)

    // Get instance
    val instance = getInstance(name)
    if (instance.isEmpty) {
      if (instanceName == name) {
        println("ERROR: Instance with name '" + name + "' could not be found.")
      } else {
        println("ERROR: Instance with name '" + instanceName + "' (remapped to '" + name + "') could not be found.")
      }
      return false  // Return Failure
    }

    // Evaluate UUID expression
    val (uuidStr, successStr) = calculateExprStr(uuidExpr, variableLUT)
    if (!successStr) {
      println ("ERROR: Could not calculate expression: " + uuidExpr)
      return false
    }

    // Attach/detach row
    if (mode == MODE_ATTACH) {
      instance.get.attachTableRow( uuidStr )
      return true
    } else if (mode == MODE_DETACH) {
      instance.get.detachTableRow( uuidStr )
      return true
    } else {
      println ("ERROR: Attach/DetachRowInstance: MODE value unrecognized: " + mode)
      return false
    }

  }



  /*
   * Variable scopes
   */
  // Add a new level of scope to the variable LUT
  def pushNewVariableScope(): Unit = {
    scopedVariableLUT = new ScopedVariableLUT( Some(scopedVariableLUT), tablestore.lexicon)
  }

  // Remove a level of scope from the variable LUT
  def popVariableScope(): Unit = {
    if (scopedVariableLUT.parentScope.isDefined) {
      scopedVariableLUT = scopedVariableLUT.parentScope.get
    }
  }

  // Reset to global scope (such as in the case of errors on code executed in the console)
  def resetToGlobalVariableScope(): Unit = {
    scopedVariableLUT = scopedVariableLUT.getGlobalScope()
  }


  /*
   * Variable Definition
   */
  def variableDefinition(name:String, expr:Expr, variableLUT:Option[VariableValueLUT]):Boolean = {
    // Step 1: Calculate the value of the variable
    val (valueDouble, valueStr, success) = calculateExpr(expr, variableLUT)
    if (!success) return false

    // Step 2: Store it
    // First check if the numeric type is defined
    if (valueDouble.isDefined) {
      scopedVariableLUT.setVariable(name, valueDouble.get.toString)
    } else {
      // Otherwise, the value is a string
      scopedVariableLUT.setVariable(name, valueStr.get)
    }

    // Return success
    true
  }


  def variableAssignment(name:String, expr:Expr, variableLUT:Option[VariableValueLUT]):Boolean = {
    if (!scopedVariableLUT.exists(name)) {
      println ("ERROR: Could not find variable named '" + name + "'.")
      return false
    }

    variableDefinition(name, expr, variableLUT)
  }

  /*
   * Conditionals (change)
   */
  // High-level function to evaluate the truth of a ConditionExpr, which may contain one or more specific kinds of
  // conditions, like left/right or change conditions, joined with zero or more operators (e.g. && and, || or).
  // Returns (condition evaluation, success/failure)
  def calculateConditionExpr(e:ConditionExpr, variableLUT:Option[VariableValueLUT]):(Boolean, Boolean) = {
    // println ("DEBUG: calculateConditionExpr: " + e.toString)
    e match {
      case ConditionElem(cond) => {

        cond match {
          case c:ConditionLR => {

            val (condEval, success) = isConditionLRTrue(c, variableLUT)
            if (!success) lastErrorStr.append("ERROR: Failed to evaluate expression: " + c.toString + "\n")
            return (condEval, success)
          }
          case c:ConditionChange => {
            val (condEval, success) = isChangeTrue(c, variableLUT)
            if (!success) lastErrorStr.append("ERROR: Failed to evaluate expression: " + c.toString + "\n")
            return (condEval, success)
          }
        }
      }
      case ConditionOperator(op, left, right) => {
        val (condEvalL, successL) = calculateConditionExpr(left, variableLUT)
        val (condEvalR, successR) = calculateConditionExpr(right, variableLUT)

        if (!successL) {
          lastErrorStr.append("ERROR: Failed to evaluate left side of expression: " + left.toString + "\n")
        }
        if (!successR) {
          lastErrorStr.append("ERROR: Failed to evaluate right side of expression: " + right.toString + "\n")
        }

        op match {
          case "&&" => {
            val condEval = condEvalL && condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case "||" => {
            val condEval = condEvalL || condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
        }
      }
    }
  }

  // Evaluation of left/right conditions (both string and double modes)
  // return (condition evaluation, success/failure)
  private def isConditionLRTrue(condLR:ConditionLR, variableLUT:Option[VariableValueLUT]):(Boolean, Boolean) = {
    val debugOutput:Boolean = false
    // TODO: Messy -- right now ConditionLRStr has generic type checking in it, and ConditionLRDouble still exists.  Should be unified into one (for dynamic typing)

    condLR match {
      case conditionLR:ConditionLRStr => {
        //println ("isConditionLRTrue: " + conditionLR.toString())
        // String mode
        val (leftDouble, leftStr, successL) = calculateExpr(conditionLR.left, variableLUT)
        if (!successL) {
          lastErrorStr.append("Calculation failed on evaluating left side. (" + conditionLR.left + ") \n")
          return (false, false)
        }
        val (rightDouble, rightStr, successR) = calculateExpr(conditionLR.right, variableLUT)
        if (!successR) {
          lastErrorStr.append("Calculation failed on evaluating right side. (" + conditionLR.right + ") \n")
          return (false, false)
        }

        if (debugOutput) println ("isConditionLRTrue:  left:'" + leftStr + "' right:'" + rightStr + "'  successL: " + successL + " successR: " + successR + "  condLR: " + conditionLR.toString())

        // Check which mode we're comparing in (both double, or both string)
        if (leftDouble.isDefined && rightDouble.isDefined) {
          // Double comparison
          conditionLR.op match {
            case "==" => (leftDouble.get == rightDouble.get, true)
            case "!=" => (leftDouble.get != rightDouble.get, true)
            case "<=" => (leftDouble.get <= rightDouble.get, true)
            case "<" => (leftDouble.get < rightDouble.get, true)
            case ">=" => (leftDouble.get >= rightDouble.get, true)
            case ">" => (leftDouble.get > rightDouble.get, true)
          }
        } else {
          // String comparison mode
          conditionLR.op match {
            case "==" => (leftStr.get == rightStr.get, true)
            case "!=" => (leftStr.get != rightStr.get, true)
            case "<=" => (leftStr.get <= rightStr.get, true)
            case "<" => (leftStr.get < rightStr.get, true)
            case ">=" => (leftStr.get >= rightStr.get, true)
            case ">" => (leftStr.get > rightStr.get, true)
          }
        }

      }
      case conditionLR:ConditionLRDouble => {
        //println ("isConditionLRTrue: " + conditionLR.toString())
        // Double mode
        val (leftDouble, successL) = calculateExprDouble(conditionLR.left, variableLUT)
        val (rightDouble, successR) = calculateExprDouble(conditionLR.right, variableLUT)

        // Check for failure
        if (!successL) {
          lastErrorStr.append("Calculation failed on evaluating left side.\n")
          return (false, false)
        }
        if (!successR) {
          lastErrorStr.append("Calculation failed on evaluating right side.\n")
          return (false, false)
        }

        conditionLR.op match {
          case "==" => (leftDouble == rightDouble, true)
          case "!=" => (leftDouble != rightDouble, true)
          case "<=" => (leftDouble <= rightDouble, true)
          case "<" => (leftDouble < rightDouble, true)
          case ">=" => (leftDouble >= rightDouble, true)
          case ">" => (leftDouble > rightDouble, true)
        }
      }
    }

  }


  // TODO: Only part-way complete
  // return (condition evaluation, success/failure)
  def isChangeTrue(condition:ConditionChange, variableLUT:Option[VariableValueLUT]):(Boolean, Boolean) = {
    // Step 1: Condition
    val _instance = getInstance(condition.instProp.name)
    if (_instance.isEmpty) {
      return (false, false)   // Failure
    }

    // Step 1B: Get reference to instance and it's current property value
    val instance = _instance.get
    if (!instance.propertyExists( condition.instProp.property )) {
      println ("ERROR: Property '" + condition.instProp.property + "' of instance '" + instance.name + "' is not populated.")
      // Exit
      return (false, false)   // Return success=failure
    }
    val propValue = instance.getProperty( condition.instProp.property ).get

    // Step 2: Check for mode of change condition
    // Vector (increases/decreases)
    val elems = condition.change.elems
    // Case 1: Both direction and threshold to pass
    if (checkForOnlyElems(Array("direction", "threshold"), elems)) {
      // Get property value as a number
      val propValueNum = propValue.getValueDouble()
      if (propValueNum.isEmpty) {
        println ("ERROR: Property '" + condition.instProp.property + "' of instance '" + instance.name + "' has value '" + propValue.value +"' that cannot be converted into a number.")
        // Exit
        return (false, false)   // Return success=failure
      }

      // Get threshold as a number
      val threshElem = getChangeElem("threshold", elems)
      if (variableLUT.isEmpty) {
        println ("ERROR: Threshold defined but variable look-up table is empty.  Conditional with change threshold may be being used outside variable scope (e.g. outside an inference pattern)")
        // Exit
        return (false, false)   // Return success=failure
      }
      //val threshValueNum = evalCellPatternToDouble( threshElem.cellPattern, variableLUT )
      val (threshValueNum, successThresh) = calculateExprDouble( threshElem.expr, variableLUT )
      if (!successThresh) {
        lastErrorStr.append("ERROR: Threshold could not be converted into a number: " + threshElem.expr.toString)
        println ("ERROR: Threshold value could not be converted to a number.")
        // Exit
        return (false, false)   // Return success=failure
      }

      // Get the direction
      val directionElem = getChangeElem("direction", elems)
      val (directionSign, success) = getDirectionSign( directionElem.expr, variableLUT )

      if (!success) {
        lastErrorStr.append("ERROR: Direction sign could not be extracted: " + directionElem.expr.toString())
        return (false, false)
      }

      // Perform the comparision
      directionSign match {
        case ">" => {
          if (propValueNum.get > threshValueNum) {
            return (true, true)
          } else {
            return (false, true)
          }
        }
        case "<" => {
          if (propValueNum.get < threshValueNum) {
            return (true, true)
          } else {
            return (false, true)
          }
        }
        case _ => {
          // Unknown direction sign
          // Exit
          return (false, false)
        }
      }


    }
    // Case 2: Direction only, no threshold
    if (checkForOnlyElems(Array("direction"), elems)) {
      // TODO
      return (false, false)
    }

    // Discrete
    // Case 3: TODO
    // Case 4: TODO


    return (false, false)
  }


  /*
   * Conditional -- Supporting functions
   * 'Expr' Expression Evaluation
   */
  // This generic first tries to calculate as a double -- if this fails, it tries to calculate as a string.
  // Allows semi-dynamic typing, keeping doubles as doubles when possible, and calculating as strings otherwise.
  def calculateExpr(e:Expr, variableLUT:Option[VariableValueLUT]):(Option[Double], Option[String], Boolean) = {
    //lastErrorStr.clear()

    // Step 1: First try to calculate as a Double
    val (valueDouble, successDouble) = calculateExprDouble(e, variableLUT)

    // Step 2: Next, try as a string
    val (valueStr, successStr) = calculateExprStr(e, variableLUT)

    if (successDouble && successStr) {
      return (Some(valueDouble), Some(valueStr), true)
    } else if (successDouble && !successStr) {
      return (Some(valueDouble), None, true)
    } else if (!successDouble && successStr) {
      return (None, Some(valueStr), true)
    }

    // Dump error
    lastErrorStr.append("Failure to calculate expression: " + e.toString + "\n")
    lastErrorStr.append(dumpVariables(variableLUT) + "\n" )

    // Default return
    return (None, None, false)
  }

  def dumpVariables(variableLUT:Option[VariableValueLUT]):String = {
    val os = new StringBuilder

    os.append ("Dump: \n")
    os.append ("ScopedVariableLUT: \n")
    os.append (scopedVariableLUT.toString + " \n")
    os.append ("\n")
    os.append (" VariableLUT: \n")
    if (variableLUT.isDefined) {
      os.append (variableLUT.get.toStringDelim("\n"))
    } else {
      os.append (" None \n")
    }
    os.append ("\n")
    os.append (" Remap: \n")
    for (remapLevel <- remap) {
      os.append ("Remap: " + remapLevel.toString() + "\n")
    }

    // Return
    os.toString()
  }

  // Evaluates an Expr and returns a Double
  private def calculateExprDouble(e:Expr, variableLUT:Option[VariableValueLUT]):(Double, Boolean) = {
    e match {
      case Number(value) => (value, true)
      case Str(value) => {
        try {
          (value.toDouble, true)
        } catch {
          case exception:NumberFormatException => {
            /*
            if (value.length == 0) {
              // If the string is blank, consider it populated with zero
              return (0.0, true)
            } else {
            */
              // String is not blank -- conversion error
              lastErrorStr.append("calculateExprDouble(): Number format exception -- cannot convert to Double: '" + value + "' on expression: " + e.toString + "\n")
              return (0.0, false)
            //}
          }
        }
      }
      case Identifier(name, statePrefix) => {
        val (value, success) = getVariableDouble(e.asInstanceOf[Identifier].name, variableLUT, statePrefix)
        return (value, success)
      }
      case InstanceProperty(name, property, statePrefix) => {
        val (value, success) = getInstancePropertyDouble(e.asInstanceOf[InstanceProperty], statePrefix)
        return (value, success)
      }
      case MeetsRequirementsInfPattern(patternName, patternMatchIdx, patternHashcode, patternConditions, remappings, temporalBlocksEnabled) => {
        val (success, meetsReq) = meetsRequirementsExprEval(patternName, patternHashcode, patternConditions, remappings, temporalBlocksEnabled, nestedInExecutePattern = false, debugDisplay = false)
        var meetsReqDouble = 0.0
        if (meetsReq) meetsReqDouble = 1.0      // Convert boolean to double
        return (meetsReqDouble, success)
      }
      case Operator(op, left, right) => {
        val (condEvalL, successL) = calculateExprDouble(left, variableLUT)
        val (condEvalR, successR) = calculateExprDouble(right, variableLUT)

        op match {
          case "*" => {
            val condEval = condEvalL * condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case "/" => {
            val condEval = condEvalL / condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case "%" => {
            val condEval = condEvalL % condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case "+" => {
            val condEval = condEvalL + condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case "-" => {
            val condEval = condEvalL - condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
        }
      }
    }
  }

  // Evaluates an Expr and returns a String, success, and also supplies an optional error string
  private def calculateExprStr(e:Expr, variableLUT:Option[VariableValueLUT]):(String, Boolean) = {
    e match {
      case Number(value) => (value.toString, true)
      case Str(value) => (value, true)
      case Identifier(name, statePrefix) => {
        val (value, success) = getVariable(e.asInstanceOf[Identifier].name, variableLUT, statePrefix)
        return (value, success)
      }
      case InstanceProperty(name, property, statePrefix) => {
        val (value, success) = getInstancePropertyStr(e.asInstanceOf[InstanceProperty], statePrefix)
        return (value, success)
      }
      case MeetsRequirementsInfPattern(patternName, patternMatchIdx, patternHashcode, patternConditions, remappings, temporalBlocksEnabled) => {
        val (success, meetsReq) = meetsRequirementsExprEval(patternName, patternHashcode, patternConditions, remappings, temporalBlocksEnabled, nestedInExecutePattern = false, debugDisplay = false)
        var meetsReqStr = "0"
        if (meetsReq) meetsReqStr = "1"      // Convert boolean to string
        return (meetsReqStr, success)
      }
      case Operator(op, left, right) => {
        val (condEvalL, successL) = calculateExprStr(left, variableLUT)
        val (condEvalR, successR) = calculateExprStr(right, variableLUT)

        op match {
          case "+" => {
            val condEval = condEvalL + condEvalR     // Operation
            val success = !(!successL || !successR)   // If either is false, then return false
            return (condEval, success)
          }
          case _ => {
            println ("ERROR: Unsupported operator (" + op + ") for string operation")
            return ("", false)
          }
        }
      }
    }
  }


  /*
   * Conditional -- Supporting functions
   * 'CellPattern' Expression Evaluation
   */

  // Evalutes a CellPattern, and returns a direction sign (greater than, less than) based on the textual content of that CellPattern.
  // This is used to convert words like 'increases', 'decreases', etc, into mathematical operators for evaluation.
  def getDirectionSign(expr:Expr, variableLUT:Option[VariableValueLUT]):(String, Boolean) = {
    val (signText, success) = calculateExprStr( expr, variableLUT )

    if (!success) return ("", false)

    if (signText == "increase") {
      return (">", true)
    }

    if (signText == "decrease") {
      return ("<", true)
    }

    // Default return
    ("", true)
  }

  // Evaluates a CellPattern, and returns a String
  def evalCellPatternToString(cellPattern:CellPattern, variableLUT:Option[VariableValueLUT]):(String, Boolean) = {
    var str = ""

    for (elem <- cellPattern.elements) {
      elem match {
        case CellText(text, isOptional, isRelaxable) => {
            str += text
        }

        case CellVariable(name, isOptional, isRelaxable) => {
          val (varStr, success) = getVariable(name, variableLUT, None)    //## Assumes no state reference possible
          if (!success) {
            // Exit
            return ("", false)   // Return success=failure
          }

          str += varStr
        }
      }

    }

    // Return
    (str, true)
  }

  // TODO: Ultimately this needs to evaluate expressions, not CellPatterns
  // Evaluates a CellPattern, and returns a Double
  def evalCellPatternToDouble(cellPattern:CellPattern, variableLUT:Option[VariableValueLUT]):Option[Double] = {
    var value:Double = 0.0

    for (elem <- cellPattern.elements) {
      elem match {
        case CellText(text, isRelaxable, isOptional) => {
          try {
            value += text.toDouble
          } catch {
            case ex:NumberFormatException => return None
          }
        }

        case CellVariable(name, isRelaxable, isOptional) => {
          val (variableValueDouble, success) = getVariableDouble( name, variableLUT, None )     //## Assumes no state reference possible
          if (!success) return None
          value += variableValueDouble
        }
      }

    }

    // Return
    Some(value)
  }



  /*
   * Conditional -- Supporting functions
   * 'ChangeElem' Helper Functions
   */

  // Returns true if 'in' contains all and *only* the operations listed in 'names'
  def checkForOnlyElems(names:Array[String], in:List[ChangeElem]):Boolean = {
    // If the sizes are different, criteria can't be true (will either be missing at least one, or have extra)
    if (names.length != in.length) return false
    // Check each element
    for (elem <- in) {
      if (!names.contains(elem.op)) return false
    }
    // Default return
    true
  }

  def hasChangeElem(name:String, in:List[ChangeElem]):Boolean = {
    for (elem <- in) {
      if (elem.op == name) return true
    }
    // Default return
    false
  }

  def getChangeElem(name:String, in:List[ChangeElem]):ChangeElem = {
    for (elem <- in) {
      if (elem.op == name) return elem
    }
    // Default return
    new ChangeElem("", new Expr())
  }


  /*
   * Meeting requirements of inference patterns
   */


  // Evaluates the MeetsRequirements call within an expression
  // Return is (success, meetsRequirements?)
  def meetsRequirementsExprEval(patternName:String, patternHashcode:Option[String], patternConditions:Option[List[ConditionExpr]], remapping:InstRemap, temporalBlocksEnabled:Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):(Boolean, Boolean) = {
    val remapInst = remapping.toRemapUni()
    val _remap = new Remap(remapInst)

    println("MeetsRequirements: RUNNING (in State " + (states.numStates - 1) + ")")

    // Populate inference pattern matches for this specific pattern and it's dependencies
    populateInferencePatternMatches(this, getPatternDependencies(patternName), maxMatches)

    // Execute pattern
    var success: Boolean = false
    var canConstraintsBeMet: Boolean = false
    if (patternHashcode.isDefined) {
      // Reference by pattern hashcode
      // TODO: nestedInExecutePattern = false here assumes this is not nested in an inference pattern
      val (success_, canConstraintsBeMet_) = meetsRequirements(patternName, patternHashcode.get, _remap, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
      success = success_
      canConstraintsBeMet = canConstraintsBeMet_
    } else if (patternConditions.isDefined) {
      // TODO: nestedInExecutePattern = false here assumes this is not nested in an inference pattern
      val (success_, canConstraintsBeMet_) = meetsRequirements(patternName, patternConditions.get, _remap, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
      success = success_
      canConstraintsBeMet = canConstraintsBeMet_
    } else {
      // Reference by pattern index
      throw new RuntimeException("MeetsRequirementsInfPattern: ERROR: Unknown method of refering to inference pattern.")
    }

    println("meetsRequirements: success: " + success)
    println("meetsRequirements: canConstraintsBeMet: " + canConstraintsBeMet)

    return (success, canConstraintsBeMet)
  }


  // Check if pattern can be executed (by hashcode reference)
  // Return is (success, meetsRequirements?)
  def meetsRequirements(patternName: String, patternHashcode: String, remapping: Remap, temporalBlocksEnabled: Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):(Boolean, Boolean) = {
    println("MeetsRequirements: Started... (patternName: " + patternName + " patterHashcode: " + patternHashcode + ")")

    println ("Remap: " + remapping.toString)
    if (!remap.isEmpty) println ("Global Remap: " + remap.mkString("\n"))

    // Step 1: Get inference pattern from name
    val inferencePatternIdx = getInferencePatternIndex(patternName)
    if (inferencePatternIdx.isEmpty) {
      println("ERROR: Could not find inference pattern with name '" + patternName + "'.")
      return (false, false) // Failure
    }
    val inferencePattern = inferencePatterns(inferencePatternIdx.get)

    // Step 2: Get the relevant pattern match
    for (patternMatch <- inferencePattern.fullPatternMatches) {
      if (patternMatch.getHashcode() == patternHashcode) {
        // Step X: Verify remap
        if (remapping.instances.size != inferencePattern.getNumInstancesRequired()) {
          println("ERROR: Inference pattern '" + inferencePattern.name + "' requires " + inferencePattern.getNumInstancesRequired() + " instance(s), but remapping specifies " + remapping.instances.size + " instance(s).")
          return (false, false) // failure
        }

        // Step 3: Check if pattern can be executed -- return true if it can, false otherwise.
        // TODO: Check for multiple possible matches, since multiple possible matches can have the same hashcode (just with different variables populated)
        val canConstraintsBeMet = patternMatch.canContraintsBeMet()
        return (true, canConstraintsBeMet)
      }
    }

    // If we reach here, then we did not find a PatternMatchInfPat with the specified hashcode
    println("ERROR: meetsRequirements(" + patternName + "): Could not find pattern match with requested hashcode (hashcode = " + patternHashcode + ")")
    lastErrorStr.append("ERROR: meetsRequirements(" + patternName + "): Could not find pattern match with requested hashcode (hashcode = " + patternHashcode + ")\n")

    return (false, false)
  }

  // Check if pattern can be executed (by a list of pattern conditions)
  // Return is (success, meetsRequirements?)
  def meetsRequirements(patternName:String, patternConditions:List[ConditionExpr], remapping:Remap, temporalBlocksEnabled:Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):(Boolean, Boolean) = {
    println("MeetsRequirements(): Started... (patternName: " + patternName + " patternConditions: " + patternConditions.mkString(", ") + ")")

    println ("Remap: " + remapping.toString)
    if (!remap.isEmpty) println ("Global Remap: " + remap.mkString("\n"))

    // Step 1: Get inference pattern from name
    val inferencePatternIdx = getInferencePatternIndex(patternName)
    if (inferencePatternIdx.isEmpty) {
      println("ERROR: Could not find inference pattern with name '" + patternName + "'.")
      return (false, false) // Failure
    }
    val inferencePattern = inferencePatterns(inferencePatternIdx.get)

    // Step 2: Get the relevant pattern match
    val matches = new ArrayBuffer[PatternMatchInfPat]
    for (patternMatch <- inferencePattern.fullPatternMatches) {
      if (checkPatternMatchMeetsConditions(patternMatch, patternConditions)) {

        // Verify remap  (println("ERROR: Inference pattern '" + inferencePattern.name + "' requires " + inferencePattern.getNumInstancesRequired() + " instance(s), but remapping specifies " + remapping.instances.size + " instance(s)."))
        if (remapping.instances.size == inferencePattern.getNumInstancesRequired()) {
          // Check if pattern can be executed
          if (patternMatch.canContraintsBeMet()) {
            matches.append(patternMatch)
          }
        }

      }
    }

    // Step 2B: Ensure that only one pattern match has been found
    if (matches.length == 0) {
      // No patterns meet requirements
      return (true, false)
    }

    // If we reach here, one or more pattern(s) meet requirements.
    // TODO: Should this be an error, or a parameter? -- PJ NOTE (06/22/2020): Now returns successfully, since this is a common situation for patterns with optional variables.  Ambiguity will have to be resolved elsewhere (e.g. executePattern())
    println("MeetsRequirements(): WARNING: More than one pattern (" + matches.length + ") meet the list of pattern conditions.  Still returning true.")
    warningStr.append("MeetsRequirements(): WARNING: More than one pattern (" + matches.length + ") meets the list of pattern conditions for (" + patternName + ").  Still returning true.\n")
    warningStr.append("\tPattern Conditions: " + patternConditions.toString() + "\n")
    return (true, true)

  }



  /*
   * Executing inference patterns
   */

  // Execute pattern (by hashcode reference)
  def executePattern(patternName: String, patternHashcode: String, remapping: Remap, temporalBlocksEnabled: Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):WalkControl = {
    println("ExecutePattern: Started... (patternName: " + patternName + " patterHashcode: " + patternHashcode + ")")

    println ("Remap: " + remapping.toString)
    if (!remap.isEmpty) println ("Global Remap: " + remap.mkString("\n"))

    // Step 1: Get inference pattern from name
    val inferencePatternIdx = getInferencePatternIndex(patternName)
    if (inferencePatternIdx.isEmpty) {
      println("ERROR: Could not find inference pattern with name '" + patternName + "'.")
      return WalkControl.mkFailure() // Failure
    }
    val inferencePattern = inferencePatterns(inferencePatternIdx.get)

    // Step 2: Get the relevant pattern match
    for (patternMatch <- inferencePattern.fullPatternMatches) {
      if (patternMatch.getHashcode() == patternHashcode) {
        // Step X: Verify remap
        if (remapping.instances.size != inferencePattern.getNumInstancesRequired()) {
          println("ERROR: Inference pattern '" + inferencePattern.name + "' requires " + inferencePattern.getNumInstancesRequired() + " instance(s), but remapping specifies " + remapping.instances.size + " instance(s).")
          return WalkControl.mkFailure() // failure
        }

        // Step 3: Call main executePattern
        return executePattern(patternMatch, remapping, temporalBlocksEnabled, nestedCall = nestedInExecutePattern, debugDisplay = debugDisplay)
      }
    }

    // If we reach here, then we did not find a PatternMatchInfPat with the specified hashcode
    println("ERROR: executePattern(" + patternName + "): Could not find pattern match with requested hashcode (hashcode = " + patternHashcode + ")")
    lastErrorStr.append("ERROR: executePattern(" + patternName + "): Could not find pattern match with requested hashcode (hashcode = " + patternHashcode + ")\n")

    return WalkControl.mkFailure()

  }

  // Execute pattern (by pattern index offset)
  def executePattern(patternName:String, patternMatchIdx:Int, remapping:Remap, temporalBlocksEnabled:Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):WalkControl = {
    println ("ExecutePattern: Started... (patternName: " + patternName + " patternMatchIdx: " + patternMatchIdx + ")")

    // Step 1: Get inference pattern from name
    val inferencePatternIdx = getInferencePatternIndex(patternName)
    if (inferencePatternIdx.isEmpty) {
      println ("ERROR: Could not find inference pattern with name '" + patternName + "'.")
      return WalkControl.mkFailure()      // Failure
    }
    val inferencePattern = inferencePatterns(inferencePatternIdx.get)

    // Step 2: Get the relevant pattern match
    if (patternMatchIdx < 0) {
      println("ERROR: patternMatchIdx must be positive. ")
      return WalkControl.mkFailure() // Failure
    }
    if (patternMatchIdx >= inferencePattern.fullPatternMatches.length) {
      println ("ERROR: patternMatchIdx exceeds the number of pattern matches found (requested: " + patternMatchIdx + "  actual number: " + inferencePattern.fullPatternMatches.length + ").")
      return WalkControl.mkFailure() // Failure
    }
    val patternMatch = inferencePattern.fullPatternMatches(patternMatchIdx)

    // Step X: Verify remap
    if (remapping.instances.size != inferencePattern.getNumInstancesRequired()) {
      println ("ERROR: Inference pattern '" + inferencePattern.name + "' requires " + inferencePattern.getNumInstancesRequired() + " instance(s), but remapping specifies " + remapping.instances.size + " instance(s).")
      return WalkControl.mkFailure() // failure
    }

    // Step 3: Call main executePattern
    executePattern(patternMatch, remapping, temporalBlocksEnabled, nestedCall = nestedInExecutePattern, debugDisplay = debugDisplay)
  }


  //## TODO: IN PROGRESS
  def checkPatternMatchMeetsConditions(in:PatternMatchInfPat, patternConditions:List[ConditionExpr]):Boolean = {
    //## TODO
    val debugDisplay:Boolean = false

    println ("*checkPatternMatchMeetsConditions: started... ")
    val fauxInterpreter = in.inferencePattern.fauxInterpreter
    fauxInterpreter.resetStates()

    // Clone instances
    val state = new InferenceState
    val instances = states.currentState.getAllInstances()
    for (instance <- instances) {
      state.addInstance( instance.cloneShallow(instance.name) )
    }
    fauxInterpreter.states.addManualState(state)


    // Check that this patternMatch meets the list of conditions specified
    for (i <- 0 until patternConditions.length) {
      if (debugDisplay) println ("Condition " + i + " / " + patternConditions.length + ": " + patternConditions(i))
      val variableLUT = in.variableValueLUT    // alternative = 0
      val (result, success) = fauxInterpreter.calculateConditionExpr(patternConditions(i), Some(variableLUT))            //## TODO: Almost certainly need a valid variableLUT here.
      if (debugDisplay) println ("\tresult: " + result + "   success: " + success)

      if ((!success) || (!result)) {
        if (debugDisplay) {
          println("Condition not met -- returning false")
          println("-----------------------------------")
          println(fauxInterpreter.lastErrorStr)
          println("-----------------------------------")
        }
        return false
      }
    }

    println ("All conditions met -- returning true")
    // If we reach here, all conditions were met
    return true
  }

  // Execute pattern (by list of conditions to be met on internal variables)
  def executePattern(patternName:String, patternConditions:List[ConditionExpr], remapping:Remap, temporalBlocksEnabled:Option[ExecTemporalBlockList], nestedInExecutePattern:Boolean, debugDisplay:Boolean):WalkControl = {
    println("ExecutePattern: Started... (patternName: " + patternName + " patternConditions: " + patternConditions.mkString(", ") + ")")

    println ("Remap: " + remapping.toString)
    if (!remap.isEmpty) println ("Global Remap: " + remap.mkString("\n"))

    // Step 1: Get inference pattern from name
    val inferencePatternIdx = getInferencePatternIndex(patternName)
    if (inferencePatternIdx.isEmpty) {
      println("ERROR: Could not find inference pattern with name '" + patternName + "'.")
      return WalkControl.mkFailure() // Failure
    }
    val inferencePattern = inferencePatterns(inferencePatternIdx.get)

    // Step 2: Get the relevant pattern match
    val matches = new ArrayBuffer[PatternMatchInfPat]
    for (patternMatch <- inferencePattern.fullPatternMatches) {
      if (checkPatternMatchMeetsConditions(patternMatch, patternConditions)) {
        matches.append(patternMatch)
      }
    }

    // Step 2B: Ensure that only one pattern match has been found
    if (matches.length == 0) {
      println("ERROR: executePattern(): No patterns meet list of pattern conditions. ")
      lastErrorStr.append("ERROR: executePattern(): No patterns meet list of pattern conditions. ")
      return WalkControl.mkFailure()
    } else if (matches.length > 1) {
      // TODO: Now choose the "best" ambiguous pattern
      println("WARNING: executePattern(): More than one pattern (" + matches.length + ") meets the list of pattern conditions.  Disambiguating by picking the match with the highest number of variables populated.")
      warningStr.append("WARNING: executePattern(): More than one pattern (" + matches.length + ") meets the list of pattern conditions. Disambiguating by picking the match with the highest number of variables populated. \n")

      // Ambiguous case -- more than one pattern meeting the criteria is found.
      // Here, we'll generally pick the pattern with the most variables populated (taking that as the "best" alternative), but also provide the user with
      // summary statistics about the pattern match distribution so they can be aware of the issue and/or further constrain their patterns.

      // Summary statistics: display distribution
      val numPopulatedVariablesCounter = new Counter[Int]
      for (pm <- matches) {
        numPopulatedVariablesCounter.incrementCount(pm.numPopulatedVariables())
      }
      val distElems = new ArrayBuffer[String]()
      var countHighest:Double = -1
      for (key <- numPopulatedVariablesCounter.keySet.toArray.sortBy(-_)) {
        distElems.append(key + ": " + numPopulatedVariablesCounter.getCount(key).toInt)
        if (countHighest == -1) countHighest = numPopulatedVariablesCounter.getCount(key)
      }
      val distStr = "(" + distElems.mkString(", ") + ")"

      warningStr.append("\tDistribution of populated variables: " + distStr + "\n")
      if (countHighest > 1) {
        warningStr.append("\tWARNING: Pattern with highest count has (" + countHighest + ") alternatives.  Picking top sorted pattern -- this may lead to non-deterministic behavior. \n")
      }

      //return WalkControl.mkFailure()
    }

    // If we reach here, there is either exactly one match, or multiple ambiguous matches.
    val sortedMatches = matches.sortBy(-_.numPopulatedVariables())    // If ambiguous match, take the one with the most variables populated
    val patternMatch = sortedMatches(0)

    // Step X: Verify remap
    if (remapping.instances.size != inferencePattern.getNumInstancesRequired()) {
      println("ERROR: Inference pattern '" + inferencePattern.name + "' requires " + inferencePattern.getNumInstancesRequired() + " instance(s), but remapping specifies " + remapping.instances.size + " instance(s).")
      return WalkControl.mkFailure() // failure
    }

    // Step 3: Call main executePattern
    return executePattern(patternMatch, remapping, temporalBlocksEnabled, nestedCall = nestedInExecutePattern, debugDisplay = debugDisplay)
  }


  // Execute pattern (main call, by reference to a specific PatternMatchInfPat object)
  def executePattern(patternMatch:PatternMatchInfPat, remapping:Remap, temporalBlocksEnabled:Option[ExecTemporalBlockList], nestedCall:Boolean, debugDisplay:Boolean):WalkControl = {
    println ("ExecutePattern: Started... (patternMatch supplied, inference pattern name: " + patternMatch.inferencePattern.name + ")")
    // Step 0A: Check if we have never executed an inference pattern before.  If so, this suggests that the current state is the
    // initial setup state.  Add a new state, so that we record the initial state as it's own separate (and first) state.
    if ((autoAddToStateSpace) && (states.numStates == 1)) {       //## TODO: Needs fixing now that executeAutoPatterns runs initially.
      states.addState()
    }

    // Step XX: Note that we executed this inference pattern in the current state
    var pmRunRecord = states.currentState.addExecutedInferencePattern(patternMatch, instRemap = remapping, wasNestedCall = nestedCall, wasAutoCall = isAutoPattern(patternMatch.inferencePattern))

    // Step 1: Get reference to InferencePattern from PatternMatch
    val inferencePattern = patternMatch.inferencePattern

    // Step 2: Set appropriate GenerateRow mode
    if (!nestedCall) {      // GenerateRow mode is inherited to component patterns in composite patterns
      if (inferencePattern.executionMode == EXECUTIONMODE_AUTOREGEN) {
        // Rows generated from patterns with the AUTOREGEN execution mode are temporary
        generateRowMode = GENERATEROW_MODE_TEMPORARY
      }
    }

    // Step 3: Check to see whether this pattern is a composite pattern or not
    val isComposite = inferencePattern.isCompositePattern()

    if (isComposite) {
      // Handle composite pattern

      // Step 3A: Retrieve remappings for each infpat in the composite pattern
      val (remapsCompInfPats, success) = patternMatch.getAllInstanceRemapsComposite(remapping)
      if (!success) {
        println ("ERROR: Could not generate remapping.  This error is likely caused by failing to include an instance, or requesting instance names that do not exist. ")
        return WalkControl.mkFailure()
      }

      // Step 3B: for each infpat in the composite pattern, process in order:
      for (infPatIdx <- 0 until inferencePattern.numInfPatInComposite()) {
        // Pattern name
        val patternMatchSlot = patternMatch.getInfPatCompositeInSlot(infPatIdx)

        // Remapping
        val remapSlot = remapsCompInfPats(infPatIdx)

        // Call executePattern() with above
        if (debugDisplay) {
          println ("Spawning ExecutePattern for infpat in composite pattern with name: " + patternMatchSlot.inferencePattern.name)
        }
        executePattern(patternMatchSlot, remapSlot, temporalBlocksEnabled, nestedCall = true, debugDisplay)
      }
    } else {
    }

    // Step 4: Get the codeblock for this inference pattern
    val codeblock = inferencePattern.codeblock

    // Step 5: Retrieve the variable list
    val variableLUTs = patternMatch.variableValueLUT

    // Step 6: Handle instance remapping
    addRemapLevel(remapping)
    pmRunRecord.instRemap = remap.last    //## Reassign the instance remapping in the PatternMatchRunRecord after it's been merged with the lower-level remappings to be executable for composite patterns.  Slightly messy to reassign it here rather than earlier.

    // Check if this remapping is valid
    // TODO: Does this work for composite patterns?  likely not implemented
    val (isValid, constraintIdx) = patternMatch.checkIfValidRemap(remapping.instances)      // NOTE: If invalid, validAltIdx will be set to 0 (so no need to reset it to zero when trying to run the pattern anyway)
    if (isValid) {
      println ("NOTE: Remap is valid")
    } else {
      println ("NOTE: Remap is invalid -- success not anticipated.")
    }


    // Step X: Calculate the shortDescription of the inference pattern that was run (which may include instance variables/pattern variables)
    var shortDescription = calculateShortDescriptionOfActiveInfPattern(patternMatch)
    // Add short description text to the run record.  (This isn't possible earlier when the record is made, as the pattern hasn't been run yet)
    pmRunRecord.setShortDescription(shortDescription)

    // Set notes for running this inference pattern
    var notes:String = ""
    if (patternMatch.inferencePattern.getNumInstancesRequired() == 0) {
      notes = " no instances required"
    } else if (isValid) {
      notes += " valid remap"
    } else {
      notes += " <font color=\"OrangeRed\"><b>constraints not satisfied<b></font> "
    }
    pmRunRecord.setNotes(notes)


    // Step 7: Run interpreter
    println(" * Starting interpreter for Inference Pattern (" + inferencePattern.name + ") patternMatch")
    val success = walkOneStep(codeblock, Some(variableLUTs), temporalBlocksEnabled, nestedInExecutePattern = true, debugDisplay)

    // Step 8: Clear instance remapping
    removeRemapLevel()
    //clearRemap()

    // Step 9: Reset GenerateRow mode
    generateRowMode = GENERATEROW_MODE_DEFAULT

    // Step 10: If enabled, store this as executed pattern one state change, and move on to the next state
    if ((!nestedCall) && (!isAutoPattern(patternMatch.inferencePattern))) {
      if (autoAddToStateSpace) {
        //states.currentState.addExplanatoryText("fromPatternCall: " + patternMatch.inferencePattern.name)
        states.addState()
      }
    }

    // Step 11: Populate inference pattern matches
    // TODO: Note this might have to be moved outside of this function, to the master call in the interpreter, to avoid potential issues with nested patterns?
    //populateInferencePatternMatches(this)

    // Return
    success
  }

  /*
   * Executing automatic inference patterns
   */
  def executeAutoPatterns(debugDisplay:Boolean = false):Boolean = {
    val quickDebug:Boolean = false      // Disables the taxonomic rule, which takes time, in exchange for faster runs to find interpreter bugs.
    val MAX_ITER:Int = 50
    println ("ExecuteAutoPatterns: Started...")


    // Step 1: Identify a list of automatic patterns
    val autoPatterns = new ArrayBuffer[InferencePattern]
    val autoPatternNames = mutable.Set[String]()
    for (infPat <- inferencePatterns) {
      if (isAutoPattern(infPat)) {
        autoPatterns.append( infPat )
        autoPatternNames.add(infPat.name)
      }
    }


    println ("ExecuteAutoPatterns: " + autoPatterns.length + " automatic patterns will be run. ")

    // Step 2: Remove all previous temporary-generated rows in the tablestore
    val numRemoved = tablestore.removeTemporaryRows(GENERATEROW_UUIDPREFIX_TEMPORARY)
    val numTableStoreRowsOriginal = tablestore.numRows
    println ("ExecuteAutoPatterns: " + numRemoved + " temporary rows removed from the TableStore.  TableStore currently has " + tablestore.numRows + " rows.")

    // Step 2A: Run the hardcoded taxonomic completion rule, which is much faster than implementing this rule through the interpreter.
    if (!quickDebug) {
      //TaxonomicCompletion.completeLinks(tablestore, TaxonomicCompletion.MODE_GENERATE_ROWS_USING_WORDS)
      TaxonomicCompletion.completeLinks(tablestore, TaxonomicCompletion.MODE_GENERATE_ROWS_USING_LEMMAS)
    }

    // Step 3: Run those patterns until the number of rows in the tablestore stabilizes
    // Blank remapping for auto-running patterns
    val remapUni = new RemapUni()
    val remap = new Remap(instances = remapUni)

    // Run each pattern match for each pattern
    var numIter:Int = 0
    var numFailures:Int = 0
    var numTableStoreRowsPrev:Int = tablestore.numRows

    breakable {
      // Step 3A: Iterate until either the number of new rows does not increase, or we reach a safety point for the maximum number of iterations.
      while (numIter < MAX_ITER) {

        numFailures = 0
        // Step 3B: For each inference pattern
        for (infPat <- autoPatterns) {
          // Step 3C: Execute each one of that inference pattern's pattern matches
          for (pm <- infPat.fullPatternMatches) {
            println ("ExecuteAutoPatterns: Running " + infPat.name + " (pm: " + pm.shortDescription + ")")
            if (pm.canContraintsBeMet()) {
              // NOTE: TemporalBlocks never enabled for automatic patterns
              val result = executePattern(pm, remap, temporalBlocksEnabled = None, nestedCall = false, debugDisplay = debugDisplay)
              if (!result.success) {
                lastErrorStr.append("ExecuteAutoPatterns(): Failed to execute pattern " + infPat.name + "\n")
                numFailures += 1
              }
            }
          }
        }

        println("ExecuteAutoPatterns: TableStore currently has " + tablestore.numRows + " rows.")

        // Step 3D: Check to see if we have NOT added any new rows -- if so, automatic patterns are no longer adding new rows, break.
        if (tablestore.numRows == numTableStoreRowsPrev) {
          break()
        } else {
          numTableStoreRowsPrev = tablestore.numRows
        }

        // Step 3E: Populate inference pattern matches based on new rows
        // TODO: For speed, could only populate the matches of patterns with automatic execution modes, until the last iteration.
        populateInferencePatternMatches(this, onlyPatternNames = autoPatternNames.toSet, maxMatches = maxMatches)


        /*
        //## Debug -- allow early exists to decrease runtime
        if (quickDebug) {
          println ("QUICKDEBUG ENABLED -- EXITING FROM EXECUTEAUTOPATTERN LOOP EARLY")
          break()
        }
         */

      }

      // If we reach here, we have hit the maximum number of iterations.  Note this to the user.
      println ("ExecuteAutoPatterns: ERROR: Maximum number of iterations has been reached (MAX_ITER = " + MAX_ITER + ").  The automatic patterns may define an infinite loop.  Exiting. ")
      return false
    }

    val numTableStoreRowsGenerated = tablestore.numRows - numTableStoreRowsOriginal
    println ("ExecuteAutoPatterns: Generated " + numTableStoreRowsGenerated + " rows from automatic patterns.  TableStore currently contains " + tablestore.numRows + " rows total. ")

    // Execute all patterns
    println ("ExecuteAutoPatterns: Populating all inference patterns... ")
    populateInferencePatternMatches(this, maxMatches = maxMatches)

    // Return
    if (numFailures > 0) {
      lastErrorStr.append("ExecuteAutoPatterns(): Failed to execute " + numFailures + " pattern(s) \n")
      return false
    } else {
      return true
    }
  }


  /*
   * Checking for/populating inference pattern matches
   */
  // Note: Interpreter reference required to get access to the InferenceStates, so that shortDescriptions can contain properties from instances (like their names).
  def populateInferencePatternMatches(mainInterpreter:Interpreter, onlyPatternNames:Set[String] = Set[String](), maxMatches:Int) = {
    val forkJoinPool = new scala.concurrent.forkjoin.ForkJoinPool(numThreads)

    println("* populateInferencePatternMatches: Started...")
    println ("onlyPatternNames: " + onlyPatternNames.mkString(", "))

    // Step 1: Clear 'populated' marker
    for (i <- 0 until inferencePatterns.length) {
      inferencePatterns(i).clearPopulated()
    }


    // Create parallel collection with variable number of threads (set to 1 thread for debugging)
    val infPatsPar = (0 until inferencePatterns.length).par
    infPatsPar.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
    var useThreads:Boolean = false
    if (numThreads > 1) useThreads = true


    // Step 2: Find patterns (row/<row variable> constraint search
    for (i <- infPatsPar) {
      if ((onlyPatternNames.isEmpty) || (onlyPatternNames.contains(inferencePatterns(i).name))) {
        println ("")
        println (" ========================================================================================= ")
        println ("")
        println ("Inference Pattern " + i + "/" + inferencePatterns.length + " (" + inferencePatterns(i).name + ")")
        inferencePatterns(i).findPatterns(tablestore, maxMatches, useThreads)
      }
    }

    // Step 3: (Non-Composite Patterns)
    println(" * processing non-composite patterns")
    // Step 3A: Check whether inference pattern matches can meet instance criteria with currently available instances in state space (if applicable).
    //##for (i <- (0 until inferencePatterns.length).par) {
    for (i <- infPatsPar) {
      if ((onlyPatternNames.isEmpty) || (onlyPatternNames.contains(inferencePatterns(i).name))) {
        if (!inferencePatterns(i).isCompositePattern()) {
          inferencePatterns(i).populateConstraints(states.currentState.getAllInstances())
          inferencePatterns(i).evaluateConstraints(tablestore, tablestore.lexicon)
          inferencePatterns(i).removePatternsOmitFlag()
          inferencePatterns(i).populatePatternMatchShortDescription(mainInterpreter)
          inferencePatterns(i).setPopulated()
        }
      }
    }

    // Step 4: Composite patterns
    println(" * processing composite patterns")
    // Iterate through composite patterns until either all have been processed, or we get stuck trying to fulfill the requirements of patterns.
    var numLast:Int = -1
    var numLeft:Int = 1
    breakable {
      while (numLeft > 0) {
        println ("Iteration")
        val patternsLeft = mutable.Set[String]()

        // Reset count for number of unprocessed composite patterns
        numLeft = 0

        //for (i <- (0 until inferencePatterns.length).par) {
        for (i <- infPatsPar) {
          if ((onlyPatternNames.isEmpty) || (onlyPatternNames.contains(inferencePatterns(i).name))) {
            // Find composite inference patterns that have not yet been populated
            if (inferencePatterns(i).isCompositePattern() && !inferencePatterns(i).isPopulated) {
              // Check to see if the requirements for this pattern have been met
              val requirementsMet = inferencePatterns(i).haveRequiredPatternsBeenPopulated(inferencePatterns)
              println(" Pattern: " + inferencePatterns(i).name + " requirementsMet: " + requirementsMet + " comp:" + inferencePatterns(i).isCompositePattern())

              println(inferencePatterns(i).compInfReq.get)
              if (requirementsMet) {
                // Do the regular instance match finding, for any instances that might be required in the composite pattern itself (and not mapped in)
                inferencePatterns(i).populateConstraints(states.currentState.getAllInstances())
                inferencePatterns(i).evaluateConstraints(tablestore, tablestore.lexicon)

                // Perform extra composite pattern matching to whittle down any options based on the composite pattern constraints
                inferencePatterns(i).patternMatchingComposite(inferencePatterns, tablestore) //## OLD

                // Remove any patterns that have been flagged as not meeting critical criteria
                inferencePatterns(i).removePatternsOmitFlag()

                // Populate short description text for each inference pattern pattern match
                inferencePatterns(i).populatePatternMatchShortDescription(this)

                // Mark that this pattern has been populated
                inferencePatterns(i).setPopulated()

              } else {
                patternsLeft += inferencePatterns(i).name
                numLeft += 1
              }

            }
          }
        }

        // Check: Ensure that at least one composite inference pattern was processed
        if ((numLeft > 0) && (numLeft == numLast)) {
          println ("ERROR: populateInferencePatternMatches(): ERROR: Composite patterns remain, but no more composite patterns were able to be processed.  This likely signifies errors in the composite patterns remaining (" + patternsLeft.mkString(", ") + ").")
          break()
        } else {
          numLast = numLeft
        }
      }
    }

    println("* populateInferencePatternMatches: Completed...")
  }


  // Get a list of the other inference patterns that a given pattern depends on.
  // Useful for computing only the subset of patterns that are required to execute a given pattern.
  // NOTE: also adds itself
  def getPatternDependencies(patternName:String, existingSet:Set[String] = Set[String]()):Set[String] = {
    val out = mutable.Set[String]()

    // Find dependencies of this pattern
    for (i <- 0 until inferencePatterns.length) {
      if (inferencePatterns(i).name == patternName) {
        out ++= inferencePatterns(i).getInfPatDependenciesComposite().toSet
      }
    }

    // For those dependencies that are new this iteration, check their dependencies too
    val newThisIteration = out.diff(existingSet)
    val combined = out.union(existingSet)
    for (newPatternName <- newThisIteration) {
      combined ++= getPatternDependencies(newPatternName, combined.toSet)
    }

    // Add self
    combined ++= Set(patternName)

    // Return
    combined.toSet
  }


  /*
   * Helper for calculating shortDescriptions of inference patterns (which may reference instance variables/pattern variables) after the fact
   */
  def calculateShortDescriptionOfActiveInfPattern(patternMatch: PatternMatchInfPat): String = {
    val patternmatchDescription = patternMatch.inferencePattern.patternmatchDescription

    // Retrieve variable LUT
    val variableLUT = patternMatch.getVariableLUT

    // Calculate value of the patternMatch Description
    val (valueDouble, valueStr, success1) = calculateExpr(patternmatchDescription, Some(variableLUT))

    // If for some reason this was unsuccessful, return an error string as the description (so it will be noticed
    if (!success1) {
      println("ERROR: Could not evaluate expression for patternmatch description '" + patternmatchDescription + "'.")
      println ("variableLUT: ")
      println (variableLUT.toStringDelim(delim = "\n"))
      return "Expression could not be evaluated: " + patternmatchDescription
    }

    // If successful, retrieve appropriately casted value
    var str: String = ""
    if (valueDouble.isDefined) {
      str = valueDouble.get.toString()
    } else {
      // Otherwise, the value is a string
      str = valueStr.get.toString()
    }

    // Return
    str
  }


  /*
   * Generating table rows
   */
  // Returns (success, uuid of new row)
  def generateRow(rowrefExpr:RowRefExpr, variableLUT:Option[VariableValueLUT]):(Boolean, String) = {
    // Step 1: Find table reference
    val tableName = rowrefExpr.getTableName()
    val _table = tablestore.getTableByName(tableName)
    if (_table.isEmpty) {
      println ("ERROR: Table with name '" + tableName + "' could not be found.")
      return (false, "")    // Failure
    }
    val table = _table.get

    // Step 2: Process cell references
    val (success, cellValueLUT) = cellPatternToMap(rowrefExpr, variableLUT)
    if (!success) return (false, "")

    // Step 3: Setup UUID Prefix
    var uuidPrefix = GENERATEROW_UUIDPREFIX_PRESISTENT
    if (generateRowMode == GENERATEROW_MODE_TEMPORARY) {
      uuidPrefix = GENERATEROW_UUIDPREFIX_TEMPORARY
    }

    // Step 4: Generate a new row
    val genRow = TableRow.mkTableRowGenerated(table, cellValueLUT.toMap, uuidPrefix)
    println ("Generated Row: " + genRow.toString)

    if (genRow.isEmpty) {
      println ("ERROR: Row could not be generated. ")
      return (false, "")      // Failure
    }

    // Step 5: Add row to table
    // Check if new row is unique
    val (isDuplicate, duplicateUID) = table.isDuplicateRow( genRow.get )
    println ("isDuplicate: " + isDuplicate)
    if (!isDuplicate) {
      // Unique row
      table.addRow( genRow.get )
      (true, genRow.get.uid)      // Success
    } else {
      // Duplicate
      (true, duplicateUID)      // Success
    }

  }


  def removeRowUUID(uuidExpr:Expr, variableLUT:Option[VariableValueLUT]):Boolean = {
    // Step 1: Evaluate UUID expression
    val (uuidStr, successStr) = calculateExprStr(uuidExpr, variableLUT)
    if (!successStr) {
      println ("ERROR: Could not calculate expression: " + uuidExpr)
      return false
    }

    // Step 2: Remove Row
    val success = tablestore.removeRow(uuidStr)
    if (!success) {
      println ("ERROR: Could not remove row with UUID ('" + uuidStr + "')")
    }

    // Return
    success
  }

  def removeRowsRowRef(rowrefExpr:RowRefExpr, variableLUT:Option[VariableValueLUT]):Boolean = {
    println (" * RemoveRowsRowRef: started... ")

    // Step 1: Find table reference
    val tableName = rowrefExpr.getTableName()
    val _table = tablestore.getTableByName(tableName)
    if (_table.isEmpty) {
      println ("ERROR: Table with name '" + tableName + "' could not be found.")
      return false    // Failure
    }
    val table = _table.get


    // Step 2: Process cell references
    val (success, cellValueLUT) = cellPatternToMap(rowrefExpr, variableLUT)
    if (!success) return false


    // Step 3: Convert cell references (as strings) to CellPatterns/PatternElems, so we can use the same constraint-matching machinery as the inference pattern row matching.
    val constraints = new ArrayBuffer[inferenceengine.struct.CellPattern]
    for (key <- cellValueLUT.keySet) {
      val colName = key
      val colPatternStr = cellValueLUT(colName)

      val elems = new ArrayBuffer[PatternElem]
      val elem = PatternElem.mkLexicalPattern(colPatternStr, tablestore.lexicon, isOptional = false, isRelaxable = false)   //## NOTE: Added isOptional/isRelaxable parameters, set to false by default -- double check this does not break the removal process. (if anything, it should provide a fairly strict removal)
      elems.append(elem)

      constraints.append( new inferenceengine.struct.CellPattern(colName, Array(elems.toArray), numVariables = Array(0), numLexicalPatterns = Array(1)) )
    }

    // Step 4: Find rows that match these constraints
    val matchingRows = inferenceengine.struct.CellPattern.getAllRowsMatchingConstraint(table, constraints.toArray, varList = new VariableList(), onlyContentTags = true)    //## TODO: populate varList correctly? (absent an inference pattern, not clear what this would be)
    println ("NOTE: varList not populated for removeRowsRowRef() -- this will likely cause errors in finding rows to remove. ")
    println ("NOTE: Constraints may be relaxed! Add code to filter relaxed constraints if not desired. ")

    println ("RemoveRows: Found " + matchingRows.length + " rows matching constraints. ")

    println(cellValueLUT.toString())
    println(constraints.toString())

    // Step 5: Display rows
    if (matchingRows.length > 0) {
      println("Removing Rows: ")
      for (row <- matchingRows) {
        println("  " + row.getRow().toStringSentWithUID())
      }
    }

    // Step 6: Remove rows
    var overallSuccess:Boolean = true
    for (row <- matchingRows) {
      val success = removeRowUUID( Str(row.getRow().uid), variableLUT)
      if (!success) overallSuccess = false
    }

    // Return
    overallSuccess
  }

  // Convert RowRefExprs to a MAP of evaluated (column, evalutaed expression as a string) pairs.
  private def cellPatternToMap(in:RowRefExpr, variableLUT:Option[VariableValueLUT]):(Boolean, Map[String, String]) = {
    val cellValueLUT = mutable.Map[String, String]()

    for (cellRef <- in.cellRefs) {
      val colName = cellRef.colName
      val (valueDouble, valueStr, success) = calculateExpr(cellRef.expr, variableLUT)
      if (!success) {
        println ("ERROR: Could not evaluate cell pattern expression for column name '" + colName + "', with expression: " + cellRef.expr )
        return (false, Map[String, String]())
      }

      // Step 2: Retrieve appropriately casted value
      var str:String = ""
      if (valueDouble.isDefined) {
        str = valueDouble.get.toString()
      } else {
        // Otherwise, the value is a string
        str = valueStr.get.toString()
      }

      // Successfully evaluted -- store
      cellValueLUT(colName) = str
    }

    // Return
    (true, cellValueLUT.toMap)
  }

  /*
   * Instantiate with common properties
   */
  def getTablestoreProperties(objName:String):Array[(Array[Int], Array[Int])] = {
    val out = new ArrayBuffer[(Array[Int], Array[Int])]   // (property, value) pairs
    println ("*getTablestoreProperties: Started (" + objName + ")")

    // Convert objName to lexicon IDs
    val objNameIdxs_ = new ArrayBuffer[Int]
    for (word <- objName.split(" ")) {
      objNameIdxs_.append( tablestore.lexicon.add(word) )
    }
    val objNameIdxs = objNameIdxs_.toArray

    // Iterate through tables
    val tableNames = tablestore.getTableNames()
    for (tableName <- tableNames) {
      // Find all tables starting with the prefix "PROP" (properties tables)
      if (tableName.startsWith("PROP")) {
        println ("Table: " + tableName)
        val table = tablestore.getTableByName(tableName).get

        // Find indicies of property, value, and agent/object columns
        val colIdxProp = table.getColumnIdxStartsWith("PROPERTY").getOrElse(-1)
        val colIdxValue = table.getColumnIdxStartsWith("VALUE").getOrElse(-1)
        var colIdxObj = table.getColumnIdxStartsWith("AGENT").getOrElse(-1)
        if (colIdxObj == -1) colIdxObj = table.getColumnIdxStartsWith("OBJECT").getOrElse(-1)

        println ("colIdxProp: " + colIdxProp + "  colIdxValue: " + colIdxValue + "  colIdxObj: " + colIdxObj)

        // If all columns have been found
        if ((colIdxObj >= 0) && (colIdxValue >= 0) && (colIdxProp >= 0)) {
          // Iterate through rows, collecting those with the appropriate objName
          for (row <- table.rows) {
            val obj = row.getCellWordsAlt(colIdxObj, 0)
            val prop = row.getCellWordsAlt(colIdxProp, 0)
            val value = row.getCellWordsAlt(colIdxValue, 0)

            println("## Table: " + tableName + "\t" + "Obj: " + TaggedLemmaHelper.getString(obj, tablestore.lexicon) + "\t" + "Prop: " + TaggedLemmaHelper.getString(prop, tablestore.lexicon) + "\t" + "Value: " + TaggedLemmaHelper.getString(value, tablestore.lexicon))

            // Check if this row's object is the same as objName
            if (TaggedLemmaHelper.compareLexiconIdxs(objNameIdxs, obj)) {
              // Match -- add to output
              out.append( (prop, value) )

              println("Table: " + tableName + "\t" + "Obj: " + TaggedLemmaHelper.getString(obj, tablestore.lexicon) + "\t" + "Prop: " + TaggedLemmaHelper.getString(prop, tablestore.lexicon) + "\t" + "Value: " + TaggedLemmaHelper.getString(value, tablestore.lexicon))
            }
          }

        }

      }
    }

    // Return
    out.toArray
  }


  def mkInstanceWithCommonProperties(name:String, kindofExpr:Expr, variableLUT:Option[VariableValueLUT], specificProperties:Array[String] = Array.empty[String]):(Boolean, ObjectInstance) = {
    // Step 1: Create an instance of the thing
    val (success1, instance) = instanceDefinition(name, kindofExpr, variableLUT)
    if (!success1) return (false, new ObjectInstance("", Array.empty[Int], tablestore) )

    val kindofIdxs = instance.kindof      // Lexicon indices for kindof expression, post-evaluation
    val kindofStr = LexiconUtil.lexiconIdxsToStr(kindofIdxs, tablestore.lexicon)


    // Step 2: Gather properties of a thing that is a kind of 'kindof' from tablestore
    val commonProps = getTablestoreProperties(kindofStr)

    println ("Common Properties (" + name + " kindof " + kindofStr + ")")
    for (i <- 0 until commonProps.length) {
      val propName = LexiconUtil.lexiconIdxsToStr(commonProps(i)._1, tablestore.lexicon)
      val propValue = LexiconUtil.lexiconIdxsToStr(commonProps(i)._2, tablestore.lexicon)
      println (i + ": " + propName + "\t" + propValue )
    }

    // Step 3: Insert the properties
    // Insert default properties
    instance.insertProperties(kindofIdxs)

    // Insert discovered properties
    for (i <- 0 until commonProps.length) {
      val propName = LexiconUtil.lexiconIdxsToStr(commonProps(i)._1, tablestore.lexicon)
      val propValue = LexiconUtil.lexiconIdxsToStr(commonProps(i)._2, tablestore.lexicon)

      if ((specificProperties.isEmpty) || (specificProperties.contains(propName))) {
        val value = new PropertyValue
        value.setDiscrete(propValue)
        instance.setProperty(propName, value)
      }
    }

    // Return
    (true, instance)
  }

  /*
   * Printing
   */
  def printStatement(expr:Expr, variableLUT:Option[VariableValueLUT]):Boolean = {
    // Step 1: Calculate the value of the expression
    val (valueDouble, valueStr, success) = calculateExpr(expr, variableLUT)
    if (!success) return false

    // Step 2: Retrieve appropriately casted value
    var str:String = ""
    if (valueDouble.isDefined) {
      str = valueDouble.get.toString()
    } else {
      // Otherwise, the value is a string
      str = valueStr.get.toString()
    }

    // Step 3: Print
    println("<<<" + str + ">>>")


    // Return
    success
  }

  def printInstances(): Unit = {
    val instances = states.currentState.getAllInstances()
    println("PrintInstances (" + instances.length + " instances found):")

    for (i <- 0 until instances.length) {
      println (i + ": " + instances(i).toString())
    }

  }

  def printVariables(): Unit = {
    println("PrintVariables (" + scopedVariableLUT.size + " variables defined):")

    println(scopedVariableLUT.toString())
  }

  // TODO: Partially implemented... currently prints to console instead of storing in (for example) the states.
  def addExplanationText(expr:Expr, variableLUT:Option[VariableValueLUT], sourceStr:String):Boolean = {

    // Step 1: Calculate the value of the expression
    val (valueDouble, valueStr, success) = calculateExpr(expr, variableLUT)
    if (!success) return false

    // Step 2: Store it
    // Step 2A: First check if the numeric type is defined
    var str:String = ""
    if (valueDouble.isDefined) {
      str = valueDouble.get.toString()
    } else {
      // Otherwise, the value is a string
      str = valueStr.get.toString()
    }

    // Step 2B: Add explanatory text to current state
    println("<<< addExplanationText: " + sourceStr + ": " + str + " >>>")
    states.currentState.addExplanatoryText( sourceStr, str )

    // Return success
    true


  }



  /*
   * Export
   */

  // Export current inference patterns and matches to an HTML log
  // Assumes that findPatterns has already taken place, so as not to wipe out any existing work on existing inference patterns (like instance matching)
  def exportInferencePatternMatchingHTML(): Unit = {
    exportInferencePatternMatchingHTML(outputPath + outputPrefix + "infpat.html")
  }

  def exportInferencePatternMatchingHTML(filename:String) {
    val htmlExporter = new IMLHtmlExport(tablestore)
    htmlExporter.addInferencePatterns( inferencePatterns.toArray )

    htmlExporter.generateHTML(filename, infPatternsToHighlight.toSet)
  }

  def setInferencePatternHighlight(in:Set[String]): Unit = {
    infPatternsToHighlight = in
  }

  // Export a human-readable version of the tablestore to an HTML file
  def exportTablestoreHTML(mode:Int): Unit = {
    exportTablestoreHTML(outputPath + outputPrefix + "tablestore.html", mode:Int)
  }

  def exportTablestoreHTML(filename:String, mode:Int) {
    val tableStoreExporter = new TableStoreHtmlExport(tablestore)
    tableStoreExporter.generateHTML(filename, mode)
  }

  // Export a human-readable version of the state space to an HTML file
  def exportStateSpace(): Unit = {
    exportStateSpace(outputPath + outputPrefix + "statespace.html")
  }

  def exportStateSpace(filename:String): Unit = {
    states.exportHTML(filename, tablestore)
  }

  def exportInferencePatternMatchesJSON(): Unit = {
    exportInferencePatternMatchesJSON(outputPath + outputPrefix + "patternmatches.json")
  }

  // Export a human-readable version of the inference pattern row slot matches for debugging purposes
  def exportInfPatRowMatchesDebug(): Unit = {
    exportInfPatRowMatchesDebug(outputPath)
  }

  def exportInfPatRowMatchesDebug(path:String): Unit = {
    /*
    for (infpat <- inferencePatterns) {
      infpat.generateDebugHTML(path + "/" + "infpatdebugexport-" + infpat.name.replaceAll("[^A-Za-z0-9]", "") + ".html", patternName = infpat.name, tablestore)
    }
    */
    var pathSafe = path
    if (path.length > 0) pathSafe = pathSafe + "/"
    InferencePattern.generateDebugHTMLAllPatterns(pathSafe + "infpatdebugexport.html", inferencePatterns.toArray, tablestore)
  }


  // Inference pattern exporting (JSON)
  def exportInferencePatternMatchesJSON(filename:String): Unit = {
    val pw = new PrintWriter(filename)

    println (" * exportInferencePatternMathcesJSON: Started... (filename = " + filename + ")")
    val jsonOutPatterns = new ArrayBuffer[String]()
    for (inferencePattern <- inferencePatterns) {
      // Get JSON for each pattern match
      val jsonMatches = new ArrayBuffer[String]()
      for (pm <- inferencePattern.fullPatternMatches) {
        jsonMatches.append( pm.toJSON() )
      }

      // Pack together into a single object representing the inference pattern
      val os = new StringBuilder
      os.append("{\n")
      os.append("\t\"patternName\":\"" + inferencePattern.name + "\",\n")
      os.append("\t\"matches\":[\n")
      os.append( jsonMatches.mkString(",\n") )
      os.append("\n")
      os.append("\t]\n")
      os.append("}")
      jsonOutPatterns.append( os.toString() )
    }

    val os1 = new StringBuilder
    os1.append("{\"patternMatches\": [\n")
    os1.append(jsonOutPatterns.mkString(",\n"))
    os1.append("\n")
    os1.append("]}\n")

    // Export to file
    pw.println(os1)

    pw.close()

    println (" * exportInferencePatternMathcesJSON: Completed... ")
  }



  /*
   * Conditional logic
   */

  // Return is (conditionSatisfied, success or failure of call/downstream calls)
  def conditionalLogic(conditionExpr:ConditionExpr, trueBranch:List[Statement], variableLUT:Option[VariableValueLUT], temporalBlocksEnabled:Option[ExecTemporalBlockList] = None, nestedInExecutePattern:Boolean = false, debugDisplay:Boolean = false):(Boolean, WalkControl) = {

    val (condValue, success) = calculateConditionExpr(conditionExpr, variableLUT)

    println("conditionalLogic: Result: " + condValue + "   success: " + success)

    if (success) {
      if (condValue == true) {
        // Push new variable scope
        pushNewVariableScope()

        val result = walkOneStep(trueBranch, variableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        println ("Returned from walkOneStep... ")

        if (result.success) {
          // Restore previous variable scope
          // Note: for failure to execute code, the variable scope is retained, but resettable by the function calling the interpreter (e.g. IMLReader)
          // This allows the calling function the option of printing a dump of the state before the failure.
          popVariableScope()
        }

        return (true, result)

      } else {
        return (false, WalkControl.mkSuccess())
      }

    } else {
      lastErrorStr.append("ERROR: conditionalLogic() returned with failure on evaluating expression: " + conditionExpr.toString() + "\n")
      return (false, WalkControl.mkFailure())
    }

  }


  /*
   * Running a list of statements
   */

  def walkOneStep(tree:List[Statement], infPatVariableLUT:Option[VariableValueLUT] = None, temporalBlocksEnabled:Option[ExecTemporalBlockList] = None, nestedInExecutePattern:Boolean = false, debugDisplay:Boolean = false):WalkControl = {
    //## Test: export debug statespace at each call of walk()
    //states.exportHTML(outputPath + outputPrefix + "statespace.html", tablestore)

    // Clear whatever the last error string may have been
    lastErrorStr.clear()

    if (debugDisplay) {
      println("walkOneStep: Started... ")
      if (!tree.isEmpty) println ("Processing ( " + tree.head + " )")
    }
    // Check if we statements to process
    if (!tree.isEmpty) {
      lastStatement = Some(tree.head)

      // Examine the first statement
      tree.head match {
        // List all possible statements and their processing bits here

        /*
         * Printing
         */
        case Print(pattern) => {
          val success = printStatement(pattern, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            return WalkControl.mkFailure()
          }
        }

        case PrintState(includeAutoPatterns) => {
          println("")
          println("-" * 50)
          print("Current State:  ")
          if (includeAutoPatterns) {
            println("(automatic patterns included)")
          } else {
            println("(no automatic patterns shown)")
          }
          println("-" * 50)
          println( states.currentState.toString(includeAutoPatterns) )
          println("-" * 50)
          println("")

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }


        case AddExplanationText(expr) => {
          val success = addExplanationText(expr, infPatVariableLUT, sourceStr = "emptysource")
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            return WalkControl.mkFailure()
          }
        }

        /*
         * Environment variables
         */
        case SetEnvironmentVariable(varName, value) => {
          varName match {
            case "maxMatches" => {
              try {
                // Set maxMatches
                maxMatches = value.toInt
              } catch {
                case _:Throwable =>
                  lastErrorStr.append("SetEnvironmentVariable(): ERROR: Could not parse integer to set maxMatches to (value = " + value + "). ")
                  return WalkControl.mkFailure()
              }
            }
            case _ => {
              lastErrorStr.append("SetEnvironmentVariable(): ERROR: Unknown environment variable (" + varName + "). ")
              return WalkControl.mkFailure()
            }
          }

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        /*
         * States
         */
        case IncrementState() => {
          states.addState()

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case SetStateName(name) => {
          states.currentState.setName(name)

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }


          //## TEST
        case GetStateWithName(name, assignToVar) => {
          val stateMatches = states.getStatesWithName(name)

          // Ensure that we have found a single state with this name
          if (stateMatches.length == 0) {
            // ERROR -- No state matches
            lastErrorStr.append("GetStateWithName: ERROR: Could not find state with name (" + name + ").  Known state names are: (" + ")")
            return WalkControl.mkFailure()
          } else if (stateMatches.length > 1) {
            // ERROR -- more than one match
            lastErrorStr.append("GetStateWithName: ERROR: State name (" + name + ") returned more than one match (" + stateMatches.length + ").")
            return WalkControl.mkFailure()
          }

          //## Test: Copy variable names from inference pattern in state.
          println ("scopedVariableLUT (before): " + scopedVariableLUT.toString)

          val state = stateMatches(0)
          val variableLUTFromState = state.getStateVariables()
          if (variableLUTFromState.isDefined) {
            scopedVariableLUT.variables.addWithPrefix(assignToVar + ".", variableLUTFromState.get)
          }

          println ("scopedVariableLUT (after): " + scopedVariableLUT.toString)

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        /*
         * Variables
         */
        // Print Variables
        case PrintVariables() => {
          printVariables()

          // Continue Walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case VariableDefinition(name, expr) => {
          val success = variableDefinition(name, expr, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            return WalkControl.mkFailure()
          }
        }

        case VariableAssignment(name, expr) => {
          val success = variableAssignment(name, expr, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            return WalkControl.mkFailure()
          }
        }

        /*
         * Instances
         */
        // Print instances
        case PrintInstances() => {
          printInstances()

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        // Instance definition (e.g. instance thing1 is a kind of substance
        case InstanceDefinition(name, kindofExpr) => {
          val (success1, instance) = instanceDefinition(name, kindofExpr, infPatVariableLUT)
          if (!success1) return WalkControl.mkFailure()

          // Attempt to add instance to current state
          val success = states.currentState.addInstance(instance)
          if (!success) {
            if (states.currentState.instanceExists(name)) {
              println("ERROR: Instance with name '" + name + "' could not be added.  The name is already in use. ")
            } else {
              println("ERROR: Instance with name '" + name + "' could not be added. ")
            }
            return WalkControl.mkFailure()
          }

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        // Instance property assignment (e.g. thing1."state of matter" = "solid")
        case InstancePropertyAssignment(instProp, expr) => {
          val success = instancePropertyAssignment(instProp, expr, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            println("ERROR: Property '" + instProp.property + "' of instance '" + instProp.name + "' could not be assigned value '" + expr.toString() + "'.")
            return WalkControl.mkFailure()
          }
        }


        case AttachRowToInstance(instanceName, uuidExpr) => {
          val success = attachDetachRowInstance(instanceName, uuidExpr:Expr, infPatVariableLUT, MODE_ATTACH)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            println ("ERROR: Could not attach row to instance. ")
            return WalkControl.mkFailure()
          }
        }

        case DetachRowFromInstance(instanceName, uuidExpr) => {
          val success = attachDetachRowInstance(instanceName, uuidExpr:Expr, infPatVariableLUT, MODE_DETACH)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            println ("ERROR: Could not detach row from instance. ")
            return WalkControl.mkFailure()
          }
        }

        /*
         * Instantiation (automatic)
         */
        // TODO: Implement "instantiation from tablestore" functions
        case InstantiateCommonProperties(name, kindof) => {
          // Create instance
          val (success1, instance) = mkInstanceWithCommonProperties(name, kindof, infPatVariableLUT)
          if (!success1) return WalkControl.mkFailure()

          // Step 4: Attempt to add instance to current state
          val success = states.currentState.addInstance(instance)
          if (!success) {
            if (states.currentState.instanceExists(name)) {
              println("ERROR: Instance with name '" + name + "' could not be added.  The name is already in use. ")
            } else {
              println("ERROR: Instance with name '" + name + "' could not be added. ")
            }
            return WalkControl.mkFailure()
          }

          //throw new RuntimeException("InstantiateCommonProperties: Currently unimplemented.")

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case InstantiateWithProperties(name, kindof, properties) => {
          // Create instance
          val (success1, instance) = mkInstanceWithCommonProperties(name, kindof, infPatVariableLUT, properties.toArray)
          if (!success1) return WalkControl.mkFailure()

          // Step 4: Attempt to add instance to current state
          val success = states.currentState.addInstance(instance)
          if (!success) {
            if (states.currentState.instanceExists(name)) {
              println("ERROR: Instance with name '" + name + "' could not be added.  The name is already in use. ")
            } else {
              println("ERROR: Instance with name '" + name + "' could not be added. ")
            }
            return WalkControl.mkFailure()
          }

          //throw new RuntimeException("InstantiateWithProperties: Currently unimplemented.")

          // Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }


        /*
         * Verify Answer
         */
        case VerifyAnswer(conditionExpr) => {
          val (condValue, success) = calculateConditionExpr(conditionExpr, infPatVariableLUT)
          if (condValue == false) {
            // The answer verification failed -- this means the script should exit with an error.
            println ("Answer verification failed: " + conditionExpr)
            lastErrorStr.append("Answer verification failed: " + conditionExpr)
            return WalkControl.mkExit(EXITCODE_ERROR_ANSVERIF)
          } else {
            println ("Answer verification successful: " + conditionExpr)
          }

          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        /*
         * Loops
         */
        case WhileLoop(conditionExpr, statements) => {
          val MAX_ITERATIONS = 25     // Maximum number of iterations of a while loop before the program throws an error (to prevent infinite loops)

          // Push new variable scope
          pushNewVariableScope()

          var numIterations:Int = 0

          var result = WalkControl.mkSuccess()      // Default result (if the loop exits immediately before iterating)
          breakable {
            while (true) {
              println("WhileLoop: Iteration: " + numIterations)
              // Check if the condition is true
              val (condValue, success) = calculateConditionExpr(conditionExpr, infPatVariableLUT)
              if (condValue == false) break() // Break if the condition is no longer true
              // Return if the condition was not successful
              if (!success) return WalkControl.mkFailure()


              result = walkOneStep(statements, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
              println("Returned from walkOneStep... ")

              // Check whether the loop ended from a break loop command
              if (result.breakLoop) {
                result = result.removeBreakLoop() // Remove the break loop, so we don't keep breaking
                break()
              }
              // Check for a hard 'exit' command
              if (result.exit) return result

              // Check whether the command ended unsuccessfully
              if (!result.success) return result

              numIterations += 1

              if (numIterations >= MAX_ITERATIONS) {
                println ("ERROR: While loop iterations (" + numIterations + ") has met the maximum number of iterations.  Exiting")
                return WalkControl.mkFailure()
              }
            }
          }

          if (result.success) {
            // Restore previous variable scope
            // Note: for failure to execute code, the variable scope is retained, but resettable by the function calling the interpreter (e.g. IMLReader)
            // This allows the calling function the option of printing a dump of the state before the failure.
            popVariableScope()
          }

          // If we reach here, then the traversal through the conditional was successful.  Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)

        }

        case Break() => {
          // This should return from the current loop
          return WalkControl.mkBreakLoop()
        }

        /*
         * Exit (Hard Execution Stop)
         */
        case Exit(code) => {
          return WalkControl.mkExit(code)
        }

        /*
         * Conditionals
         */
        // If Change Conditional (e.g. if (thing1.temperature increases beyond the boiling point) then
        //case IfStatement(conditionExpr, trueBranch, falseBranch) => {
        case IfStatement(conditions) => {

          breakable {
            for (condition <- conditions) {
              condition.conditionalMode match {
                case "IF" => {
                  val (returnValue, result) = conditionalLogic(condition.conditionExpr.get, condition.trueBranch, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
                  if (!result.success) return WalkControl.mkFailure()
                  if (result.breakLoop) return result
                  if (result.exit) return result
                  if (returnValue == true) break()
                }
                case "ELSEIF" => {
                  val (returnValue, result) = conditionalLogic(condition.conditionExpr.get, condition.trueBranch, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
                  if (!result.success) return WalkControl.mkFailure()
                  if (result.breakLoop) return result
                  if (result.exit) return result
                  if (returnValue == true) break()
                }
                case "ELSE" => {
                  // If we reach here, then run the else block
                  val result = walkOneStep(condition.trueBranch, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
                  if (!result.success) return WalkControl.mkFailure()
                  if (result.breakLoop) return result
                  if (result.exit) return result
                  break()
                }
              }
            }
          }

          // If we reach here, then the traversal through the conditional was successful.  Continue walking
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)

        }


        /*
         * Executing patterns
         */
        case ExecuteInfPattern(patternName, patternMatchIdx, patternHashcode, patternConditions, remappings, temporalBlocksEnabled) => {
          val remapInst = remappings.toRemapUni()
          val _remap = new Remap(remapInst)

          // Populate inference pattern matches for this specific pattern and it's dependencies
          val patternDependencies = getPatternDependencies(patternName)
          println ("Dependencies for pattern (" + patternName + "): " + patternDependencies.mkString(", "))
          populateInferencePatternMatches(this, patternDependencies, maxMatches)

          // Execute pattern
          var result = WalkControl.mkFailure()    // Default, in case pattern can't be executed
          if (patternHashcode.isDefined) {
            // Reference by pattern hashcode
            result = executePattern(patternName, patternHashcode.get, _remap, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else if (patternMatchIdx.isDefined) {
            // Reference by pattern index
            result = executePattern(patternName, patternMatchIdx.get, _remap, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else if (patternConditions.isDefined) {
            // Reference by list of conditions to be met on internal variables
            result = executePattern(patternName, patternConditions.get, _remap, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            // Unknown method of pattern reference -- this should never happen.
            throw new RuntimeException("ExecuteInfPattern: No method of reference was specified for executing an inference pattern match (pattern index, hashcode, and conditions were all blank).")
          }

          if (result.success) {
            if (result.breakLoop) return result
            if (result.exit) return result

            //## Test: export statespace to html
            exportStateSpace()

            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {

            return WalkControl.mkFailure()
          }
        }

        case ExecuteAutoPatterns() => {
          val success = executeAutoPatterns()
          if (success) {
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            return WalkControl.mkFailure()
          }
        }


        /*
         * Populating inference pattern matches
         */
        case PopulateInfPatMatches() => {
          populateInferencePatternMatches(this, maxMatches = maxMatches)
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        /*
         * Temporal Blocks
         */
        case TemporalBlock(mode, statements) => {
          println ("TemporalBlock found (mode = " + mode + ").  TemporalBlocks enabled: " + temporalBlocksEnabled.get.elems)

          if (temporalBlocksEnabled.isEmpty) {
            println ("WARNING: TemporalBlock found (mode = " + mode + "), but TemporalBlocks are not currently enabled. ")
          } else {
            if (temporalBlocksEnabled.get.isEnabled( mode )) {
              println ("EXECUTING MODE " + mode)
              // The mode of this block is in the list of enabled temporal modes -- execute it
              val result = walkOneStep(statements, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
              if (!result.success) {
                return WalkControl.mkFailure()
              }
            } else {
              println ("FALSE")
            }
          }

          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        /*
         * GenerateRow
         */
        case GenerateRow(rowrefExpr, variableNameForUUID) => {
          val (success, rowUUID) = generateRow(rowrefExpr, infPatVariableLUT)

          if (success) {
            if (variableNameForUUID.isDefined) {
              // Case 1: Also assign the UUID of the generated row to a variable
              var variableName = variableNameForUUID.get
              var value = Str(rowUUID)
              val success1 = variableDefinition(variableName, value, infPatVariableLUT)
              if (success1) {
                return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
              } else {
                return WalkControl.mkFailure()
              }
            } else {
              // Case 2: No assignment, continue walking
              return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
            }
          } else {
            return WalkControl.mkFailure()
          }
        }

        case RemoveRowUUID(uuidExpr) => {
          val success = removeRowUUID(uuidExpr, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            println ("ERROR: Could not remove row with requested UUID (Expr: '" + uuidExpr + "'")
            return WalkControl.mkFailure()
          }
        }

        case RemoveRowsRowRef(rowrefExpr) => {
          val success = removeRowsRowRef(rowrefExpr, infPatVariableLUT)
          if (success) {
            // Continue walking
            return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
          } else {
            println ("ERROR: Could not remove row with requested row reference expression (Expr: '" + rowrefExpr + "'")
            return WalkControl.mkFailure()
          }
        }


        /*
         * Exports
         */
        case ExportInferencePatternsHTML(filename) => {
          if (filename.length > 0) {
            exportInferencePatternMatchingHTML(filename)
          } else {
            // Default filename
            exportInferencePatternMatchingHTML()
          }
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case ExportInferencePatternsJSON(filename) => {
          if (filename.length > 0) {
            exportInferencePatternMatchesJSON(filename)
          } else {
            // Default filename
            exportInferencePatternMatchesJSON()
          }
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case ExportTableStoreHTML(filename) => {
          if (filename.length > 0) {
            exportTablestoreHTML(filename, TableStoreHtmlExport.MODE_LEMMAS)
          } else {
            // Default filename
            exportTablestoreHTML(TableStoreHtmlExport.MODE_LEMMAS)
          }
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case ExportStateSpaceHTML(filename) => {
          if (filename.length > 0) {
            exportStateSpace(filename)
          } else {
            // Default filename
            exportStateSpace()
          }
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }

        case ExportInferencePatternDebugHTML(path) => {
          if (path.length > 0) {
            exportInfPatRowMatchesDebug(path)
          } else {
            // Default filename
            exportInfPatRowMatchesDebug()
          }
          return walkOneStep(tree.tail, infPatVariableLUT, temporalBlocksEnabled, nestedInExecutePattern, debugDisplay)
        }


        /*
         * Catch-all
         */
        // Unknown statement/catch-all
        case x:Statement => {
          println ("ERROR: Encountered unknown or unimplemented statement: " + x.toString())
          return WalkControl.mkFailure()
        }

      }


    } else {
      // No statements to process -- return success
      lastStatement = None

      // Return
      return WalkControl.mkSuccess()
    }

  }


}


object Interpreter {
  // GenerateRow modes
  val GENERATEROW_MODE_PERSISTANT   = 1
  val GENERATEROW_MODE_TEMPORARY    = 2

  val GENERATEROW_MODE_DEFAULT      = GENERATEROW_MODE_PERSISTANT

  val GENERATEROW_UUIDPREFIX_PRESISTENT   =   "GEN-"
  val GENERATEROW_UUIDPREFIX_TEMPORARY    =   "TEMPGEN-"


  // Modes: Attach/Detach Table Row from Instance
  val MODE_ATTACH   =   1
  val MODE_DETACH   =   2


  // Exit Codes:
  val EXITCODE_ERROR_ANSVERIF   =     -10


  // Return true if an inference pattern is designed to be run automatically
  def isAutoPattern(infPat:InferencePattern):Boolean = {
    if ((infPat.executionMode == EXECUTIONMODE_AUTO) || (infPat.executionMode == EXECUTIONMODE_AUTOREGEN)) return true
    // Default return
    return false
  }

}