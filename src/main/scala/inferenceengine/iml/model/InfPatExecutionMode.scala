package inferenceengine.iml.model

/**
  * Created by user on 8/24/18.
  */
case class InfPatExecutionMode(mode:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("InfPatExecutionMode(mode: " + mode + ")")

    // Return
    os.toString()
  }

}
