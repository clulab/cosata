package inferenceengine.iml.model

/**
  * Created by peter on 7/9/18.
  */
case class Function(name:String, arguments:Map[String, Int], statements:List[Statement], val returnValue:Expr) {

}
