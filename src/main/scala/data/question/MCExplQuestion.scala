package data.question

import explanationgraph.TableStore
import inferenceengine.struct.PatternMatchInfPat

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Storage class for an MCQuestion and a tablestore explanation
  * Created by peter on 7/9/17.
  */
class MCExplQuestion(val question:MCQuestion) {
  val expl = new ArrayBuffer[ExplanationRow]

  /*
   * Constructor
   */
  parseExplanationString( question.explanation )


  /*
   * Explanation String Parsing
   */
  def parseExplanationString(in:String): Unit = {
    if (in.length < 1) return

    var uidTuples = in.trim().toUpperCase.split(" ")
    for (uidTuple <- uidTuples) {
      val fields = uidTuple.split("\\|")
      val uid = fields(0).toLowerCase
      val role = fields(1)

      breakable {
        for (i <- 0 until expl.length) {
          if (expl(i).uid == uid) {
            println (" * WARNING: Duplicate UIDs found in question.  Keeping only the first instance. (" + question.toStringMinimal + ") (UID = " + uid + "). ")
            break()
          }
        }
        expl.append(new ExplanationRow(uid, role))
      }
    }
  }

  /*
   * Explanation comparison
   */
  def getExplanationUUIDs():Set[String] = {
    val out = mutable.Set[String]()
    for (explRow <- expl) {
      out.add(explRow.uid)
    }
    // Return
    out.toSet
  }

  def getExplanationUUIDs(roleFilter:Array[String]):Set[String] = {
    val out = mutable.Set[String]()

    // If the role filter is empty, return all explanation UUIDs
    if (roleFilter.size == 0) return getExplanationUUIDs()

    for (explRow <- expl) {
      if (roleFilter.contains(explRow.role.toUpperCase)) {
        out.add(explRow.uid)
      }
    }
    // Return
    out.toSet
  }

  def getRoleOfUUID(uuid:String):String = {
    for (i <- 0 until expl.length) {
      if (expl(i).uid == uuid) return expl(i).role
    }
    // Default return
    return ""
  }

  /*
   * String methods
   */
  def toString(tablestore:TableStore):String = {
    val os = new StringBuilder
    os.append( question.toString() + "\n" )

    os.append("Explanation (Table Rows):\n")
    for (i <- 0 until expl.size) {
      os.append("\t" + expl(i).uid + " \t" + expl(i).role + "\t" + tablestore.getRowByUID(expl(i).uid).toStringText() + "\n")
    }

    os.toString()
  }

  override def toString():String = {
    val os = new StringBuilder
    os.append( question.toString() + "\n" )

    os.append("Explanation (Table Rows):\n")
    for (i <- 0 until expl.size) {
      os.append("\t" + expl(i).uid + " \t" + expl(i).role + "\n")
    }

    os.toString()
  }

}


// Storage class
class ExplanationRow(val uid:String, val role:String) {

  // Determine equality (based on UID, and not role)
  override def equals(that:Any):Boolean = {
    that match {
      case that:ExplanationRow => if (this.uid == that.uid) true else false
      case _ => false
    }
  }


}