package inferenceengine.iml.model

/**
  * Created by peter on 7/10/18.
  */
case class VariableConstantText(name:String, value:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("CONST: " + name + " = " + value)

    // Return
    os.toString()
  }

}


