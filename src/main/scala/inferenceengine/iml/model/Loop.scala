package inferenceengine.iml.model

/**
  * Created by peter on 3/22/19.
  */


case class WhileLoop(conditionExpr:ConditionExpr, statements:List[Statement]) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("WhileLoop(")
    os.append(" conditionExpr:" + conditionExpr + "\n")
    os.append(" statements: \n" + statements.mkString("\n   ") )
    os.append(")\n")


    // Return
    os.toString()
  }

}

case class Break() extends Statement
