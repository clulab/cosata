package inferenceengine.iml.model

/**
  * Created by peter on 9/18/18.
  */
case class AttachRowToInstance(instanceName:String, uuidExpr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("AttachRowToInstance(instanceName: " + instanceName + ", uuidExpr: " + uuidExpr.toString() + ")")

    // Return
    os.toString()
  }

}

case class DetachRowFromInstance(instanceName:String, uuidExpr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("DetachRowFromInstance(instanceName: " + instanceName + ", uuidExpr: " + uuidExpr.toString() + ")")

    // Return
    os.toString()
  }

}
