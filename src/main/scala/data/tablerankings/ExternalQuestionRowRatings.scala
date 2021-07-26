package data.tablerankings

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ExternalQuestionRowRatings(val ratings:Map[String, Double], val questionID:String, filename:String) {

  def getRating(uuid:String):Option[Double] = {
    if (!ratings.contains(uuid)) return None

    ratings.get(uuid)
  }


  def fullyRated():Boolean = {
    var numNotRated:Int = 0
    for (key <- ratings.keySet) {
      if (ratings(key) == 0) numNotRated += 1
    }

    if (numNotRated <= 1) {   // Allow up to 1 to be not rated
      return true
    }

    // Default return
    false
  }


  /*
   * String methods
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("External Ratings (QID: " + questionID + ")" + "\n")
    for (key <- ratings.keySet) {
      os.append (key + "\t" + ratings(key) + "\n")
    }

    // Return
    os.toString()
  }

}



object ExternalQuestionRowRatings {

  def loadRatingsFromSpreadsheet(filename:String, delim:String = "\t"):Map[String, ExternalQuestionRowRatings] = {
    val out = new mutable.HashMap[String, ArrayBuffer[(String, Double)]]()    // (UUID, Rating)

    var lineCount: Int = 0
    var linesUntilNextQ = 3
    var curQID:String = ""

    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      println ("lineCount: " + lineCount + "\t linesUntilNextQ: " + linesUntilNextQ + "\t " + line)

      if (lineCount < 3) {
        // Header -- disregard

      } else {

        val fields = line.split(delim)

        if (linesUntilNextQ == 0) {
          // At top of current question
          curQID = fields(0)
          if (!out.contains(curQID)) {
            out(curQID) = new ArrayBuffer[(String, Double)]
          }

        } else if (linesUntilNextQ == -1) {
          // Question data
          if (fields(0).length == 0) {
            // Disregard -- answer candidates
          } else if (fields(0).startsWith("Accuracy")) {
            // Disregard -- performance
            linesUntilNextQ = 5     // next question will begin in 4 lines
            curQID = ""

          } else {
            val manualRating = fields(1).toDouble
            val rowText = fields(2)
            val uuid = fields(3)

            out(curQID).append( (uuid, manualRating) )
          }

        } else {
          // In between questions

        }

      }

      lineCount += 1
      if (linesUntilNextQ >= 0) {
        linesUntilNextQ -= 1
      }
    }

    // Repack
    val repacked = mutable.Map[String, ExternalQuestionRowRatings]()
    for (qid <- out.keySet) {
      val ratingMap = out(qid).toMap[String, Double]
      val packed = new ExternalQuestionRowRatings(ratingMap, qid, filename)
      repacked(qid) = packed
    }

    // Return
    repacked.toMap
  }


  /*
   * Testing
   */
  def main(args:Array[String]): Unit = {
    val data = loadRatingsFromSpreadsheet("explregen/ExplanationRegeneration-ManualWorldtreeRatings-Apr24-2020-devonly.tsv")

    for (key <- data.keySet) {
      println (key)
      println (data(key).toString())
      println (data(key).fullyRated())

      println ("")
    }
  }


}



