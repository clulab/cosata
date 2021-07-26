package data.question

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Created by peter on 7/9/17.
  */

object ExamQuestionUtils {

  /*
   * Supporting functions
   */

  def convertToExplQuestions(in:Array[MCQuestion]):Array[MCExplQuestion] = {
    val out = new ArrayBuffer[MCExplQuestion]
    for (i <- 0 until in.size) {
      out.append( new MCExplQuestion(in(i)) )
    }
    // Return
    out.toArray
  }


  // Take a set of questions, and return only the questions that have a non-zero value on one or more flags.
  def filterQuestionsByFlags(in:Array[MCExplQuestion], flags:Array[String]):Array[MCExplQuestion] = {
    val out = new ArrayBuffer[MCExplQuestion]

    for (i <- 0 until in.size) {
      breakable {
        for (flag <- flags) {
          if (in(i).question.flags.getCount(flag) > 0) {
            out.append(in(i))
          }
        }
      }
    }

    // Return
    out.toArray
  }

}
