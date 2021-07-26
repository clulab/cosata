package inferenceengine.iml.model

/**
  * Created by user on 7/29/18.
  */
case class Print(expr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("Print(Expr:" + expr + ")")

    // Return
    os.toString()
  }

}


case class PrintInstances() extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("PrintInstances()")

    // Return
    os.toString()
  }

}


case class PrintVariables() extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("PrintVariables()")

    // Return
    os.toString()
  }

}


case class PrintState(includeAutoPatterns:Boolean) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("PrintState(includeAutomaticPatterns = " + includeAutoPatterns + ")")

    // Return
    os.toString()
  }

}