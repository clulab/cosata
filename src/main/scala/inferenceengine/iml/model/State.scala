package inferenceengine.iml.model

import scala.util.parsing.input.Positional

/**
  * Created by user on 7/29/18.
  */

/*
 * Manually increment state
 */
case class IncrementState() extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("IncrementState()")

    // Return
    os.toString()
  }

}


/*
 * Set state name
 */

case class SetStateName(name:String) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("SetStateName(name = " + name + ")")

    // Return
    os.toString()
  }

}


/*
 * State getters
 */

case class GetStateWithName(name:String, assignToVar:String) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("GetStateWithName(name = " + name + ", assignToVar = " + assignToVar + ")")

    // Return
    os.toString()
  }

}


/*
 * State references
 */

class StateRef extends Positional {

}


class StateWithName(val name:String) extends StateRef {

  override def toString():String = {
    val os = new StringBuilder

    os.append("StateWithName(name: " + name + ")")

    // Return
    os.toString()
  }

}

class StateFirstWithName(val name:String) extends StateRef {

  override def toString():String = {
    val os = new StringBuilder

    os.append("StateFirstWithName(name: " + name + ")")

    // Return
    os.toString()
  }

}

class StateLastWithName(val name:String) extends StateRef {

  override def toString():String = {
    val os = new StringBuilder

    os.append("StateLastWithName(name: " + name + ")")

    // Return
    os.toString()
  }

}

class StateBeforeName(val name:String) extends StateRef {

  override def toString():String = {
    val os = new StringBuilder

    os.append("StateBeforeName(name: " + name + ")")

    // Return
    os.toString()
  }

}

class StateAfterName(val name:String) extends StateRef {

  override def toString():String = {
    val os = new StringBuilder

    os.append("StateAfterName(name: " + name + ")")

    // Return
    os.toString()
  }

}
