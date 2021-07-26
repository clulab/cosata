package inferenceengine.iml.model

/**
  * Created by user on 7/26/18.
  */
case class InstancePropertyAssignment(instanceProperty:InstanceProperty, expr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstancePropertyAssignment(" + instanceProperty + ", expr: " + expr + ")")

    // Return
    os.toString()
  }

}

