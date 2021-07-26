package inferenceengine.iml.model

/**
  * Created by peter on 7/27/18.
  */

// Condition types
class Condition() {

}

class ConditionLR() extends Condition {

}

case class ConditionLRStr(val op:String, val left:Expr, val right:Expr) extends ConditionLR {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("ConditionLRStr(left: " + left.toString() + " operator: " + op.toString() + " right: " + right.toString() + ")")

    // Return
    os.toString()

  }

}

case class ConditionLRDouble(val op:String, val left:Expr, val right:Expr) extends ConditionLR {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("ConditionLRDouble(left: " + left.toString() + " operator: " + op.toString() + " right: " + right.toString() + ")")

    // Return
    os.toString()

  }

}


case class ConditionChange(instProp:InstanceProperty, change:Change) extends Condition {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("ConditionChange(InstanceProperty: " + instProp.toString() + ", Change: " + change.toString() + ")")

    // Return
    os.toString()

  }

}


// Expressions of conditions
class ConditionExpr() {
  /*
  override def toString():String = {
    this match {
      case x:ConditionOperator => x.toString()
      case x:ConditionElem => x.toString()
      case _ => "Unknown ConditionExpr"
    }
  }
  */

}

case class ConditionOperator(op:String, var left:ConditionExpr, var right:ConditionExpr) extends ConditionExpr {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("ConditionOperator(op: " + op + " left: " + left + " right: " + right + ")")

    // Return
    os.toString()
  }

}

case class ConditionElem(condition:Condition) extends ConditionExpr {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("ConditionElem(" + condition + ")")

    // Return
    os.toString()
  }

}

