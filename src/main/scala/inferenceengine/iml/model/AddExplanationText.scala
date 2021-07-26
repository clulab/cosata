package inferenceengine.iml.model

/**
  * Created by user on 9/13/18.
  */
case class AddExplanationText(expr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("AddExplanationText(" + expr + ")")

    // Return
    os.toString()
  }

}
