package inferenceengine.iml.model

import inferenceengine.struct.InferencePattern.{EXECUTIONMODE_NORMAL, EXECUTIONMODE_AUTO, EXECUTIONMODE_AUTOREGEN, executionModeMap, validExecutionModes}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by peter on 7/9/18.
  */
case class InfPattern(name:String, description:String, patternDescription:PatternMatchDescription, executionMode:Int, requirements:Option[InfInstanceReq], compReq:Option[CompInfPatReq], constraints:List[InfConstraint], constants:List[VariableConstantText], rows:List[RowDef], code:List[Statement]) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("Inference Pattern Name: " + name + "\n")
    os.append("Description: " + description + "\n")
    os.append("ExecutionMode: " + executionMode + "\n")

    os.append("Requirements:\n")
    os.append("\t" + requirements.toString() )
    os.append("\n")

    os.append("Composite Inference Pattern Requirements:\n")
    os.append("\t" + compReq.toString() )
    os.append("\n")

    os.append("Constraints (" + constraints.length + " total):\n")
    for (i <- 0 until constraints.length) {
      os.append("\t" + i + ": " + constraints(i).toString() + "\n")
    }


    os.append("Constants (" + constants.size + " total):\n ")
    for (i <- 0 until constants.length) {
      os.append("\t" + i + ": " + constants(i).toString + "\n")
    }

    os.append("Rows (" + rows.size + " total): \n")
    for (i <- 0 until rows.length) {
      os.append("\t" + i + ": " + rows(i).toString + "\n")
    }

    os.append("Code (" + code.size + " total statements): \n")
    for (i <- 0 until code.length) {
      os.append("\t" + i + ": " + code(i).toString + "\n")
    }

    // Return
    os.toString()
  }

}

object InfPattern {

  // TODO: Work in progress
  def mkInfPattern(name:String, infStatements:List[StatementInf], statements:List[Statement]):InfPattern = {
    // name:String, description:String, requirements:Option[InfInstanceReq], compReq:Option[CompInfPatReq], constraints:List[InfConstraint], constants:List[VariableConstantText],
    //  rows:List[RowDef], code:List[Statement]
    //##
    // Group statements

    // Description
    val descriptions = infStatements collect { case si:InfPatDescription => si }
    if (descriptions.length > 1) {
      throw new RuntimeException("ERROR: Inference pattern '" + name + "' has more than one 'description'.")
    }
    var description = ""
    if (descriptions.length == 1) description = descriptions(0).desc

    // Pattern description
    val patternDescriptions = infStatements collect { case si:PatternMatchDescription => si }
    if (patternDescriptions.length > 1) {
      throw new RuntimeException("ERROR: Inference pattern '" + name + "' has more than one 'patterndescription'.")
    }
    var patternDescription = new PatternMatchDescription(new Str("No description available"))
    if (patternDescriptions.length == 1) patternDescription = patternDescriptions(0)


    // Execution Mode
    var executionMode = EXECUTIONMODE_NORMAL      // Default
    val executionModes = infStatements collect { case si:InfPatExecutionMode => si }
    if (executionModes.length > 1) {
      throw new RuntimeException("ERROR: Inference pattern '" + name + "' has more than one 'exectionmode'.")
    }
    if (executionModes.length == 1) {
      if (!validExecutionModes.contains(executionModes(0).mode)) {
        throw new RuntimeException("ERROR: Inference pattern '" + name + "' contains an unknown execution mode: '" + executionModes(0).mode + "'. Valid modes are: " + validExecutionModes.mkString(", ") + ".")
      }
      executionMode = executionModeMap( executionModes(0).mode )
    }


    // Constants
    val constants = infStatements collect { case si:VariableConstantText => si }

    // Row definitions
    val rowdefs = infStatements collect { case si:RowDef => si }

    // TODO: Add the rest of the inference pattern components

    // Constraints

    // Instance requirements and constraints
    val instRequirements = infStatements collect { case si:InstanceRequirement => si }
    var infInstReq:Option[InfInstanceReq] = None
    if (instRequirements.length > 0) {
      infInstReq = Some( new InfInstanceReq(instances = instRequirements) )
    }


    // Composite pattern requirements
    val compInfPatRefs = infStatements collect { case si:CompInfPatRef => si }
    val compRowEquiv = infStatements collect { case si:CompRowEquivalency => si }
    val compInstEquivalency = infStatements collect { case si:CompInstEquivalency => si }

    var compInfPatReq:Option[CompInfPatReq] = None
    if (compInfPatRefs.length > 0) {
      compInfPatReq = Some( new CompInfPatReq(infPatReq = compInfPatRefs, rowEquiv = compRowEquiv, instEquiv = compInstEquivalency) )
    }



    // General constraints (that do not include instances)
    val constraints = infStatements collect { case si:InfConstraint => si }



    // Codeblock to run if inference pattern is activated
    val code = statements

    new InfPattern(name = name, description = description, patternDescription = patternDescription, executionMode = executionMode, requirements = infInstReq, compReq = compInfPatReq, constraints = constraints, constants = constants,
                    rows = rowdefs, code = code)
  }
}


// Inference Instance Requirements
//case class InfInstanceReq(instances:List[InstanceRequirement], constraints:List[InfConstraint]) {
case class InfInstanceReq(instances:List[InstanceRequirement]) {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InfInstanceReq(Instances: " + instances + ")")

    // Return
    os.toString()

  }

}

class InfConstraint() extends StatementInf {

}

/*
case class InstanceConstraintShouldHave(instanceName:String, condExpr:ConditionExpr) extends ConstraintInstance {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InstanceConstraintShouldHave(InstanceName:" + instanceName + " condExpr:" + condExpr + ")")

    // Return
    os.toString()

  }

}

case class InstanceConstraintMustHave(instanceName:String, condExpr:ConditionExpr) extends ConstraintInstance {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InstanceConstraintMustHave(InstanceName:" + instanceName + " condExpr:" + condExpr + ")")

    // Return
    os.toString()

  }

}
*/

case class InfConstraintShouldHave(condExpr:ConditionExpr) extends InfConstraint {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InfConstraintShouldHave(condExpr:" + condExpr + ")")

    // Return
    os.toString()

  }

}

case class InfConstraintMustHave(condExpr:ConditionExpr) extends InfConstraint {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InfConstraintMustHave(condExpr:" + condExpr + ")")

    // Return
    os.toString()

  }

}

case class InfConstraintMustHaveOrOmit(condExpr:ConditionExpr) extends InfConstraint {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("InfConstraintMustHaveOrOmit(condExpr:" + condExpr + ")")

    // Return
    os.toString()

  }

}

/*
 * Composite inference patterns
 */

case class CompInfPatReq(infPatReq:List[CompInfPatRef], rowEquiv:List[CompRowEquivalency], instEquiv:List[CompInstEquivalency]) {

  // Check to see if this pattern is empty
  def isEmpty:Boolean = {
    if (infPatReq.isEmpty && rowEquiv.isEmpty && instEquiv.isEmpty) return true
    // Otherwise
    false
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("CompInfPatReq(infPatReq: " + infPatReq.toString + " rowEquiv: " + rowEquiv.toString + " instEquiv: " + instEquiv.toString + ")")

    // Return
    os.toString()
  }

}

// A reference/definition for an inference pattern to be used/required in a composite inference pattern.
// e.g. infpat x ChangeOfState
// for this, 'x' is the 'referenceName' (a variable name for this instance of the pattern), and 'ChangeOfState' is the 'patternName'
case class CompInfPatRef(referenceName:String, patternName:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("CompInfPatRef(referenceName: " + referenceName + " patternName: " + patternName + ")")

    // Return
    os.toString()
  }

}

// An equivalence between rows in different inference pattern references
// e.g. rowequiv x.rowname1 = y.rowname2
// This means x.rowname must have the same UUID as y.rowname
// 'ident1:x.rowname1", 'ident2:y.rowname2'
case class CompRowEquivalency(ident1:String, ident2:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("CompRowEquivalency(ident1: " + ident1 + " ident2: " + ident2 + ")")

    // Return
    os.toString()
  }

}

// An equivalence between two instances -- one in the current inference pattern ('intName1'), the other in an inference pattern brought into a composite pattern (patternRefName1, instName2)
// e.g. instmap x = y.instname
case class CompInstEquivalency(instName1:String, patternRefName2:String, instName2:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("CompInstEquivalency(instName1: " + instName1 + " <--> patternRefName2: " + patternRefName2 + " instName2: " + instName2 + ")")

    // Return
    os.toString()
  }

}