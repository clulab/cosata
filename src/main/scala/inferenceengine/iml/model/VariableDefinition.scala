package inferenceengine.iml.model

/**
  * Created by peter on 7/9/18.
  */
case class VariableDefinition(name:String, value:Expr) extends Statement {

}

case class VariableAssignment(name:String, value:Expr) extends Statement {

}
