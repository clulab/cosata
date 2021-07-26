package inferenceengine.iml.model

/**
  * Created by user on 3/22/20.
  */

case class SetEnvironmentVariable(varName:String, value:String) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("SetEnvironmentVariable(varName: " + varName + ", value: " + value + ")")

    // Return
    os.toString
  }

}


