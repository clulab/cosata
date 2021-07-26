package inferenceengine.iml.runtime

import inferenceengine.iml.parser.IMLParser

import scala.io.Source

/**
  * Created by peter on 7/9/18.
  */
class IMLTestEntryPoint {

}

object IMLTestEntryPoint {

  def fileToString(filename:String) = {
    val file = Source.fromFile(filename)
    val str = file.mkString
    // Return
    str
  }

  def main(args:Array[String]) = {
    val script = fileToString("scripts/test.iml")

    val parser = new IMLParser

    parser.parseAll(parser.program, script) match {
      case parser.Success(r, n) => {
        // Succesfully parsed program
        println (r.toString)

        println ("Successfully parsed program. ")
      }

      case parser.Error(msg, n) => println("Error: " + msg)
      case parser.Failure(msg, n) => println("Error: " + msg)
      case _ =>
    }

  }


}
