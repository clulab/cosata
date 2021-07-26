package inferenceengine.iml.model

import inferenceengine.struct.RemapUni

/**
  * Instance Remap
  * Created by user on 8/2/18.
  */
case class InstRemap(remappings:List[(String, String)]) {

  def toRemapUni():RemapUni = {
    val remap = new RemapUni

    for (pair <- remappings) {
      remap.addRemap( pair._1, pair._2 )      // find in Pattern, replace with State
    }

    // Return
    remap
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstRemap: " + remappings)

    // Return
    os.toString()
  }

}


// A list of temporal blocks that should be enabled/executed when an inference pattern is run by executePattern
case class ExecTemporalBlockList(elems:List[ExecTemporalElem]) {

  def isEnabled(mode:String):Boolean = {
    for (elem <- elems) {
      if (elem.name == mode) return true
    }
    // Default return
    false
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("ExecTemporalList(")
    os.append( elems.mkString(", ") )
    os.append(")")

    // Return
    os.toString()
  }

}

case class ExecTemporalElem(name:String) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("ExecTemporalElem(" + name + ")")

    // Return
    os.toString()
  }

}
