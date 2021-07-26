package inferenceengine.iml.model

/**
  * Created by peter on 3/23/19.
  */
case class VerifyAnswer(conditionExpr:ConditionExpr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("VerifyAnswer(Condition: " + conditionExpr + ")")

    // Return
    os.toString
  }

}
