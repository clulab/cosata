package inferenceengine.iml.model

import scala.util.parsing.input.Positional

/**
  * Created by peter on 7/9/18.
  */
class Expr extends Positional {

  // Returns true if Identifier 'name' is used in this expression, either as an identifier, or as the instance name in an InstanceProperty reference.
  def contains(name:String):Boolean = {

    this match {
      case e:Identifier => {
        if (e.name == name) return true
      }
      case e:InstanceProperty => {
        if (e.name == name) return true
      }
    }

    // Default return
    false
  }

}

case class Number(value:Double) extends Expr {
  override def toString:String = "Double(value: " + value + ")"
}

case class Str(value:String) extends Expr {
  override def toString:String = "String(value:'" + value + "')"
}

case class Operator(op:String, var left:Expr, var right:Expr) extends Expr {
  // Returns true if Identifier 'name' is used in this expression, either as an identifier, or as the instance name in an InstanceProperty reference.
  override def contains(name:String):Boolean = {
    if (left.contains(name) == true) return true
    if (right.contains(name) == true) return true
    // Default return
    false
  }

  override def toString:String = {
    return "Operator(left: " + left.toString + "operator: " + op + " right: " + right + ")"
  }

}

case class Identifier(name:String, state:Option[StateRef]) extends Expr {

  override def toString():String = {
    val os = new StringBuilder

    os.append("Identifier(name: " + name + ", state: " + state.getOrElse("default") + ")")

    // Return
    os.toString()
  }

}

case class InstanceProperty(name:String, property:String, state:Option[StateRef]) extends Expr {

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstanceProperty(name: " + name + ", property: " + property + ", state: " + state.getOrElse("default") + ")")

    // Return
    os.toString()
  }

}
