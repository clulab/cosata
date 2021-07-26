package inferenceengine.struct

/**
  * Storage class for storing an evaluation of how well a given instance meets the requirements for an instnace pattern's InfInstanceReq
  * Created by user on 7/31/18.
  */

class ConstraintReqEval() {

}

// existingInstanceName is the name of an instance in the state space being evaluted
// patternInstanceName is the name of a required instance in the inference pattern that would be mapped to the existingInstanceName.
class InstanceReqEval(val existingInstanceName:String, val patternInstanceName:String, val existingInstance:ObjectInstance, val constraintsProcessed:Array[Boolean], val kindOf:Double, val mustHave:Double, val shouldHave:Double) extends ConstraintReqEval {

  def meetsKindOfReq:Boolean = {
    if (kindOf == 1.0) return true
    // Otherwise
    false
  }

  def meetsRequirements:Boolean = {
    if ((kindOf == 1.0) && (mustHave == 1.0)) return true
    if ((kindOf == 1.0) && (mustHave.isNaN)) return true

    // Otherwise
    false
  }

  // Returns true if there are unprocessed constraints in this pattern, potentially signifying that either the data wasn't available yet (such as for composite patterns), or that the constraints have errors in them (i.e. a variable name that doesn't exist)
  def hasUnprocessedConstraints:Boolean = {
    for (i <- 0 until constraintsProcessed.length) {
      if (constraintsProcessed(i) == false) return true
    }
    // Default return
    false
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("InstanceReqEval(existingInstance: " + existingInstanceName + " patternInstanceName: " + patternInstanceName + " ")
    os.append("kindOf: " + kindOf.formatted("%3.3f") + " ")
    os.append("mustHave: " + mustHave.formatted("%3.3f") + " ")
    os.append("shouldHave: " + shouldHave.formatted("%3.3f") + ")")

    // Return
    os.toString
  }

}

class OverallInstanceReqEval(val evals:Array[InstanceReqEval]) extends ConstraintReqEval {

  // Returns true if all instance evals in this combination meet requirements
  def meetsRequirements:Boolean = {
    for (eval <- evals) {
      if (!eval.meetsRequirements) return false
    }

    // Otherwise
    true
  }

  // Returns true if there are unprocessed constraints in any of these evaluations
  def hasUnprocessessedConstraints:Boolean = {
    for (eval <- evals) {
      if (eval.hasUnprocessedConstraints) return true
    }
    // Default return
    false
  }

  /*
   * Cloning
   */
  override def clone():OverallInstanceReqEval = {
    // Shallow copy
    new OverallInstanceReqEval(evals)
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("OverallInstanceReqEval(")
    for (i <- 0 until evals.length) {
      os.append(i + ": " + evals(i))
      if (i < evals.length-1) os.append(", ")
    }
    os.append(", hasUnprocessedConstraints: " + hasUnprocessessedConstraints)
    os.append(")")



    // Return
    os.toString
  }
}


class OverallConstraintReqEval(val mustHave:Double, val shouldHave:Double) extends ConstraintReqEval {

  def meetsRequirements:Boolean = {
    if (mustHave == 1.0) return true
    if (mustHave.isNaN) return true

    // Otherwise
    false
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("OverallConstraintReqEval(")
    os.append("mustHave: " + mustHave.formatted("%3.3f") + " ")
    os.append("shouldHave: " + shouldHave.formatted("%3.3f") + ")")

    // Return
    os.toString
  }

}
