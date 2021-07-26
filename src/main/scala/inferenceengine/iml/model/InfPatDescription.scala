package inferenceengine.iml.model

/**
  * Created by user on 8/20/18.
  */
case class InfPatDescription(desc:String) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("InfPatDescription(desc: " + desc + ")")

    // Return
    os.toString()
  }

}


case class PatternMatchDescription(expr:Expr) extends StatementInf {

  override def toString():String = {
    val os = new StringBuilder

    os.append("PatternMatchDescription(Expr:" + expr + ")")

    // Return
    os.toString()
  }

}
