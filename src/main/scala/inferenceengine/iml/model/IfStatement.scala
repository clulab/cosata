package inferenceengine.iml.model

/**
  * Created by peter on 7/27/18.
  */

/*
case class IfStatement(conditionExpr:ConditionExpr, trueBranch:List[Statement], falseBranch:List[Statement]) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("IfStatement(ConditionExpr:" + conditionExpr + "):\n")
    os.append("\tTrueBranch:\n")
    for (statement <- trueBranch) {
      os.append("\t\t" + statement + "\n")
    }
    os.append("\tFalseBranch:\n")
    for (statement <- falseBranch) {
      os.append("\t\t" + statement + "\n")
    }

    // Return
    os.toString()

  }

}
*/
case class IfStatement(conditions:List[IfStatementCondition]) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("IfStatement(Conditions: \n")
    for (i <- 0 until conditions.length) {
      os.append("Condition " + i + ": " + conditions(i).toString() + "\n")
    }

    // Return
    os.toString()
  }

}

// One comparison in an if statement (conditionalMode should be IF, ELSEIF, or ELSE)
case class IfStatementCondition(conditionalMode:String, conditionExpr:Option[ConditionExpr], trueBranch:List[Statement]) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("IfStatementCondition(conditionalMode: " + conditionalMode + ", ConditionExpr:" + conditionExpr + "):\n")
    os.append("\tTrueBranch:\n")
    for (statement <- trueBranch) {
      os.append("\t\t" + statement + "\n")
    }

    // Return
    os.toString()

  }

}