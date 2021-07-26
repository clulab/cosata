package inferenceengine.iml.runtime

/**
  * Storage class and generators for program flow control (loop breaks, program exits), or regular successful returns
  * Created by peter on 3/22/19.
  */
class WalkControl(val success:Boolean, val breakLoop:Boolean, val exit:Boolean, val exitCode:Int) {

  // Remove break loop
  def removeBreakLoop():WalkControl = {
    new WalkControl(success, false, exit, exitCode)
  }

  override def toString:String = {
    val os = new StringBuilder

    os.append("(")
    os.append("success = " + success + ", ")
    os.append("breakloop = " + breakLoop + ", ")
    os.append("exit = " + exit + ", ")
    os.append("exitCode = " + exitCode)
    os.append(")")

    // Return
    os.toString()
  }

}

object WalkControl {

  // Generators
  def mkSuccess():WalkControl = {
    new WalkControl(success = true, breakLoop = false, exit = false, exitCode = 0)
  }

  def mkBreakLoop():WalkControl = {
    new WalkControl(success = true, breakLoop = true, exit = false, exitCode = 0)
  }

  def mkExit(exitCode:Int):WalkControl = {
    new WalkControl(success = false, breakLoop = false, exit = false, exitCode = exitCode)
  }



  def mkFailure():WalkControl = {
    new WalkControl(success = false, breakLoop = false, exit = false, exitCode = 0)
  }
}
