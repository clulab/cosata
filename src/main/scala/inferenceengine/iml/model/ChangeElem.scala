package inferenceengine.iml.model

/**
  * Created by peter on 7/27/18.
  */
case class ChangeElem(op:String, expr:Expr) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("ChangeElem(op: " + op + " expr: " + expr + ")")

    // Return
    os.toString()

  }


}
