package inferenceengine.struct

import inferenceengine.iml.model.{InfConstraint, InfConstraintMustHave, InfConstraintMustHaveOrOmit, InfConstraintShouldHave}
import ConstraintEval._

/**
  * Storage classes for evaluating inference pattern constraints
  * Created by peter on 8/23/18.
  */

// Storage class for a single constraint
class ConstraintEval(val constraint:InfConstraint) {
  // A boolean representing whether this constraint has been met or not
  var constraintMet:Boolean = false

  // A boolean representing whether this constraint has been successfully processed or not (i.e. not yet processed, or met with errors during processing).
  var processed:Boolean = false


  /*
   * Determining whether the constraint has been met or not
   */
  def setConstraintMet() {
    constraintMet = true
  }

  def setConstraintUnmet() {
    constraintMet = false
  }

  def isConstraintMet():Boolean = {
    // Return
    constraintMet
  }


  /*
   * Processed
   */
  def setProcessed() {
    processed = true
  }

  def setUnprocessed(): Unit = {
    processed = false
  }

  // Has this constraint been processed successfully?
  def isProcessed():Boolean = {
    // Return
    processed
  }


  /*
   * Type determination
   */
  // Determine whether this is a SHOULDHAVE or MUSTHAVE constraint
  def getConstraintType():Int = {
    constraint match {
      case c:InfConstraintShouldHave => return CONSTRAINT_SHOULDHAVE
      case c:InfConstraintMustHave => return CONSTRAINT_MUSTHAVE
      case c:InfConstraintMustHaveOrOmit => return CONSTRAINT_MUSTHAVEOROMIT
      case _ => throw new RuntimeException("ERROR: ConstraintEval: Constraint type not recognized. ")
    }
  }

  /*
   * Cloning
   */
  override def clone():ConstraintEval = {
    // Shallow copy on constraint list
    val out = new ConstraintEval(constraint)

    out.constraintMet = this.constraintMet
    out.processed = this.processed

    // Return
    out
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    // TODO
    os.append("[met = " + isConstraintMet() + ",  processed = " + isProcessed() + ",  constraint = " + constraint.toString() + "]")

    // Return
    os.toString()
  }

}

object ConstraintEval {
  // Constraint constants
  val CONSTRAINT_SHOULDHAVE     = 1
  val CONSTRAINT_MUSTHAVE       = 2
  val CONSTRAINT_MUSTHAVEOROMIT = 3

}


// Storage class for an instance mapping from the state space to an inference pattern.
class InstanceMapEval(val existingInstanceName:String, val patternInstanceName:String, val existingInstance:ObjectInstance, var kindOfMet:Boolean = false) {

  /*
   * Accessors/Setters for KindOf matching requirements
   */
  def setKindOfMet(): Unit = {
    kindOfMet = true
  }

  def setKindOfUnmet(): Unit = {
    kindOfMet = false
  }

  // Check if this instance mapping's kindOf type requirements have been met
  def isKindOfRequirementMet():Boolean = {
    // Return
    kindOfMet
  }


  /*
   * Cloning
   */
  override def clone():InstanceMapEval = {
    new InstanceMapEval(existingInstanceName, patternInstanceName, existingInstance, kindOfMet)
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    // TODO
    os.append("InstanceMap(" + existingInstanceName + " -> " + patternInstanceName + "  [kindOfMet: " + kindOfMet + "])")

    // Return
    os.toString()
  }

}


// Storage class for a complete set of constraint evaluations
class OverallConstraintEval(val instanceMaps:Array[InstanceMapEval], val evals:Array[ConstraintEval]) {


  /*
   * Accessing constraints
   */
  def numConstraints():Int = {
    // Return
    evals.length
  }

  def getConstraintEval(idx:Int):ConstraintEval = {
    // Return
    evals(idx)
  }


  /*
   * Instance requirements
   */
  def hasInstanceRequirements():Boolean = {
    if (instanceMaps.length > 0) return true
    // Default return
    false
  }

  def checkInstanceKindofRequirementsMet():Boolean = {
    for (eval <- instanceMaps) {
      if (!eval.isKindOfRequirementMet()) return false
    }
    // Default return
    true
  }


  /*
   * Unprocessed constraints
   */

  // Check to see if there is at least one unprocessed constraint
  def hasUnprocessedConstraints():Boolean = {
    for (eval <- evals) {
      if (!eval.isProcessed()) return true
    }
    // Default return
    false
  }


  /*
   * Scoring
   */

  // Get the overall score for this pattern (
  def getScore(CONSTRAINT_TYPE:Int):Double = {
    var sum:Double = 0.0
    var numCount:Double = 0.0

    for (eval <- evals) {
      if (eval.getConstraintType() == CONSTRAINT_TYPE) {
        // If the constraint is met, add one to the sum
        if (eval.isConstraintMet()) {
          sum += 1.0
        }
        numCount += 1.0
      }
    }

    // Return score representing the proportion of constraints of a given type having been met.
    sum / numCount
  }


  /*
   * Checking all requirements to see if this satisfies all constraints
   */
  def satisfiesConstraints():Boolean = {
    // Step 1: Check satisfies instance kindOf requirements
    if (!checkInstanceKindofRequirementsMet()) return false

    // Step 2: Check there are no unprocessed constraints
    if (hasUnprocessedConstraints()) return false

    // Step 3: Check there are no MUSTHAVEOROMIT cases.
    if (shouldOmit()) return false

    // Step 4: Check the MUSTHAVE score is either 1.0 (all) or NaN (has no musthave constraints)
    val musthaveScore = getScore(CONSTRAINT_MUSTHAVE)
    if (!((musthaveScore == 1.0) || (musthaveScore.isNaN))) return false


    // If we reach here, then the constraints have been successfully met
    // Return
    true
  }

  // Check to see if MustHaveOrOmit constraints have not been met, and this pattern should be omitted
  def shouldOmit():Boolean = {
    for (eval <- evals) {
      if (eval.isProcessed()) {
        if (eval.constraint.isInstanceOf[InfConstraintMustHaveOrOmit]) {
          if (!eval.isConstraintMet()) {
            return true // A MustHaveOrOmit constraint has not been met.
          }
        }
      }
    }

    // Default return
    false
  }

  /*
   * Generating remaps from instance constraints
   */
  def getRemap():Remap = {
    val out = new RemapUni()

    // Step 1: Populate a RemapUni with the pattern remapping information
    for (instMap <- instanceMaps) {
      out.addRemap(instMap.patternInstanceName, instMap.existingInstanceName)
    }

    // Return
    new Remap(instances = out)
  }

  /*
   * Cloning
   */
  override def clone():OverallConstraintEval = {
    val im = instanceMaps.map(_.clone())
    val ev = evals.map(_.clone())
    new OverallConstraintEval(im, ev)
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    // TODO
    os.append("OverallConstraintEval (" + instanceMaps.length + " instance maps, " + evals.length + " constraints)\n")
    os.append("\tInstance maps:\n")
    for (i <- 0 until instanceMaps.length) {
      os.append("\t\t" + i + ": \t" + instanceMaps(i).toString() + "\n")
    }

    os.append("\tConstraints:\n")
    for (i <- 0 until evals.length) {
      os.append("\t\t" + i + ": \t" + evals(i).toString() + "\n")
    }

    // Return
    os.toString()
  }
}
