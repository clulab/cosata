package inferenceengine.iml.model

/**
  * Created by peter on 7/27/18.
  */
case class IfChangeStatement(condition:ConditionChange, trueBranch:List[Statement], falseBranch:List[Statement]) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("IfChangeStatement(" + condition + "):\n")
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
