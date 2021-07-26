package data.tablerankings

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Storage class for loading external rankigns of tablestore rows for given questions generated using other external methods (e.g. BERT) that
  * have been saved to disk, and might be used for various experiments (e.g. explanation regeneration).
  * Created by peter on 4/3/2020.
  */

class ExternalTableRowRankings(val scoresMap:Map[String, Double], val scoresRanked:Array[(String, Double)], val questionID:String, val filename:String) {
  val rankedUUIDs = mkUUIDRanks(scoresRanked)

  // Accessors
  def getScore(uuid:String):Option[Double] = {
    if (!scoresMap.contains(uuid)) return None
    // Return
    scoresMap.get(uuid)
  }

  def getRank(uuid:String):Option[Int] = {
    for (i <- 0 until scoresRanked.length) {
      if (scoresRanked(i)._1 == uuid) return Some(i)
    }
    // Default return
    None
  }

  def size:Int = scoresRanked.length

  // Calculate UUID ranks
  private def mkUUIDRanks(rankedScores:Array[(String, Double)]):Map[String, Int] = {
    val out = new mutable.HashMap[String, Int]
    for (i <- 0 until rankedScores.length) {
      val uuid = rankedScores(i)._1
      out(uuid) = i
    }
    // Return
    out.toMap
  }

  /*
   * String methods
   */
  def toString(maxToDisplay:Int = 10):String = {
    val os = new StringBuilder

    os.append("qid: " + questionID + "\n")
    os.append("filename: " + filename + "\n")

    for (i <- 0 until math.min(scoresRanked.length, maxToDisplay)) {
      val uuid = scoresRanked(i)._1
      val score = scoresRanked(i)._2
      os.append(i + "\t" + uuid + "\t" + score + "\n")
    }

    os.toString()
  }

  override def toString():String = {
    this.toString(maxToDisplay = 10)
  }

}


object ExternalTableRowRankings {

  def loadBertRankings(filename:String, questionID:String, delim:String = "\t"):ExternalTableRowRankings = {
    val scoresOut = mutable.HashMap[String, Double]()
    val orderedOut = new ArrayBuffer[(String, Double)]()

    // println (" * loadBertRankings: started... (filename = " + filename + ")")

    try {
      var lineCount: Int = 0
      for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
        if (lineCount == 0) {
          // Header
          val headerFields = line.split(delim)

          // Header verification
          val expectedHeaderRows = Array("uuid", "rowText", "isGoldRow", "score_0", "score_1")
          var isError: Boolean = false
          for (i <- 0 until expectedHeaderRows.length) {
            if (headerFields(i) != expectedHeaderRows(i)) {
              println("ERROR: Header column (" + i + "), expected column header (" + expectedHeaderRows(i) + ") differs from read column header (" + headerFields(i) + ")")
              isError = true
            }
          }
          if (isError) {
            throw new RuntimeException(" * loadBertRankings(): ERROR: Header format unexpected (filename = " + filename + ")")
          }

        } else {
          // Data
          val fields = line.split(delim)

          val uuid = fields(0)
          val score = fields(4).toDouble

          // Add to map
          scoresOut(uuid) = score
          // Add to array
          orderedOut.append((uuid, score))
        }

        lineCount += 1
      }

      // Sort
      val sortedOut = orderedOut.sortBy(- _._2)

      // Return
      new ExternalTableRowRankings(scoresOut.toMap, sortedOut.toArray, questionID, filename)

    } catch {
      case _:Exception => {
        println("ERROR: Could not process file ( " + filename + " ). Returning blank list of ranks. ")

        if (!filename.endsWith("_ENUM0.tsv")) {
          val split = filename.split("\\.")
          println("filename: " + filename)
          println("split: " + split.toList)
          val filenameAlt = split.slice(0, split.length - 1).mkString(".") + "_ENUM0" + "." + split(split.length - 1)
          val alternative = loadBertRankings(filenameAlt, questionID, delim)

          if (alternative.scoresRanked.length > 0) {
            println("WARNING: Did find alternate file: " + filenameAlt + " -- using this. ")
            return alternative
          }
        }
        return  new ExternalTableRowRankings(scoresOut.toMap, orderedOut.toArray, questionID, filename)
      }
    }

  }


  def loadRobertaRankingsOyvind(filename:String, questionID:String, delim:String = "\t"):ExternalTableRowRankings = {
    // One big file
    val scoresOut = mutable.HashMap[String, Double]()
    val orderedOut = new ArrayBuffer[(String, Double)]()

    //println (" * loadRobertaRankingsOyvind: started... (filename = " + filename + ")")

    try {

      var lineCount: Int = 0
      breakable {
        for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
          if (lineCount == 0) {
            // Header
            val headerFields = line.split(delim)

            // Header verification
            val expectedHeaderRows = Array("QuestionID", "ExplanationID", "Score", "IsGold")
            var isError: Boolean = false
            for (i <- 0 until expectedHeaderRows.length) {
              if (headerFields(i) != expectedHeaderRows(i)) {
                println("ERROR: Header column (" + i + "), expected column header (" + expectedHeaderRows(i) + ") differs from read column header (" + headerFields(i) + ")")
                isError = true
              }
            }
            if (isError) {
              throw new RuntimeException(" * loadRobertaRankingsOyvind(): ERROR: Header format unexpected (filename = " + filename + ")")
            }

          } else {
            // Data
            val fields = line.split(delim)

            val qid = fields(0)
            val uuid = fields(1)
            val score = fields(2).toDouble

            if (qid == questionID) {
              // Add to map
              scoresOut(uuid) = score
              // Add to array
              orderedOut.append((uuid, score))
            }
          }

          lineCount += 1
        }
      }

      if (orderedOut.length == 0) {
        println("ERROR: Did not find ratings for question ID (" + questionID + ") in file (" + filename + ").")
      }

      // Sort
      val sortedOut = orderedOut.sortBy(- _._2)

      // Return
      new ExternalTableRowRankings(scoresOut.toMap, sortedOut.toArray, questionID, filename)

    } catch {
      case _:Exception => {
        println ("ERROR: Could not process file ( " + filename + " ). Returning blank list of ranks. ")
        new ExternalTableRowRankings(scoresOut.toMap, orderedOut.toArray, questionID, filename)
      }
    }

  }

  def loadRobertaRankingsOyvindPeter(filename:String, questionID:String, delim:String = "\t"):ExternalTableRowRankings = {
    // One big file
    val scoresOut = mutable.HashMap[String, Double]()
    val orderedOut = new ArrayBuffer[(String, Double)]()

    //println (" * loadRobertaRankingsOyvind: started... (filename = " + filename + ")")

    try {

      var lineCount: Int = 0
      breakable {
        for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
          if (lineCount == -1) {      // NOTE: No header
            // Header
            val headerFields = line.split(delim)

            // Header verification
            val expectedHeaderRows = Array("QuestionID", "ExplanationID", "Score", "IsGold")
            var isError: Boolean = false
            for (i <- 0 until expectedHeaderRows.length) {
              if (headerFields(i) != expectedHeaderRows(i)) {
                println("ERROR: Header column (" + i + "), expected column header (" + expectedHeaderRows(i) + ") differs from read column header (" + headerFields(i) + ")")
                isError = true
              }
            }
            if (isError) {
              throw new RuntimeException(" * loadRobertaRankingsOyvindPeter(): ERROR: Header format unexpected (filename = " + filename + ")")
            }

          } else {
            // Data
            val fields = line.split(delim)

            val uuid = fields(0)
            val score = fields(3).toDouble

            // Add to map
            scoresOut(uuid) = score
            // Add to array
            orderedOut.append( (uuid, score) )

          }

          lineCount += 1
        }
      }

      if (orderedOut.length == 0) {
        println("ERROR: Did not find ratings for question ID (" + questionID + ") in file (" + filename + ").")
      }

      // Sort
      val sortedOut = orderedOut.sortBy(- _._2)

      // Return
      new ExternalTableRowRankings(scoresOut.toMap, sortedOut.toArray, questionID, filename)

    } catch {
      case _:Exception => {
        println ("ERROR: Could not process file ( " + filename + " ). Returning blank list of ranks. ")
        new ExternalTableRowRankings(scoresOut.toMap, orderedOut.toArray, questionID, filename)
      }
    }

  }

  def loadTfIdfRankings(filename:String, questionID:String, delim:String = "\t"):ExternalTableRowRankings = {
    val scoresOut = mutable.HashMap[String, Double]()
    val orderedOut = new ArrayBuffer[(String, Double)]()

    println (" * loadTfIdfRankings: started... (filename = " + filename + ")")

    var scoreColIdx:Int = -1
    var uuidColIdx:Int = -1

    try {
      var lineCount: Int = 0
      for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
        if (lineCount == 0) {
          // Header
          val headerFields = line.split(delim)

          // Header verification
          val expectedHeaderRows = Array("COS_AC_CORRECT", "COS_Q", "UUID", "RowText")
          var isError: Boolean = false
          for (i <- 0 until expectedHeaderRows.length) {
            if (!headerFields.contains(expectedHeaderRows(i))) {
              println("ERROR: Expected column header (" + expectedHeaderRows(i) + ") not found in column header (" + headerFields.mkString(", ") + ")")
              isError = true
            }
          }
          if (isError) {
            throw new RuntimeException(" * loadTfIdfRankings(): ERROR: Header format unexpected (filename = " + filename + ")")
          }

          // Header parsing
          scoreColIdx = headerFields.indexOf("COS_AC_CORRECT")
          uuidColIdx = headerFields.indexOf("UUID")

        } else {
          // Data
          val fields = line.split(delim)

          val uuid = fields(uuidColIdx)
          val score = fields(scoreColIdx).toDouble

          // Add to map
          scoresOut(uuid) = score
          // Add to array
          orderedOut.append((uuid, score))
        }

        lineCount += 1
      }

      // Sort
      val sortedOut = orderedOut.sortBy(- _._2)

      // Return
      new ExternalTableRowRankings(scoresOut.toMap, sortedOut.toArray, questionID, filename)

    } catch {
      case _:Exception => {
        println ("ERROR: Could not process file ( " + filename + " ). Returning blank list of ranks. ")
        new ExternalTableRowRankings(scoresOut.toMap, orderedOut.toArray, questionID, filename)
      }
    }

  }




  /*
   * Preprocessing to convert Oyvind's single large file of rankings to a series of smaller files (for quicker loading at runtime, so they don't all have to be kept in memory at once)
   */
  def chunkRobertaOyvind(path:String, filename:String, delim:String = "\t"): Unit = {
    val questionLines = new mutable.HashMap[String, ArrayBuffer[String]]()

    // Step 1: Read file
    var headerLine:String = ""
      var lineCount: Int = 0
      breakable {
        for (line <- io.Source.fromFile(path + "/" + filename, "UTF-8").getLines()) {
          if (lineCount == 0) {
            // Header
            headerLine = line
            val headerFields = line.split(delim)

            // Header verification
            val expectedHeaderRows = Array("QuestionID", "ExplanationID", "Score", "IsGold")
            var isError: Boolean = false
            for (i <- 0 until expectedHeaderRows.length) {
              if (headerFields(i) != expectedHeaderRows(i)) {
                println("ERROR: Header column (" + i + "), expected column header (" + expectedHeaderRows(i) + ") differs from read column header (" + headerFields(i) + ")")
                isError = true
              }
            }
            if (isError) {
              throw new RuntimeException(" * loadBertRankings(): ERROR: Header format unexpected (filename = " + filename + ")")
            }

          } else {
            // Data
            val fields = line.split(delim)

            val qid = fields(0)
            val uuid = fields(1)
            val score = fields(2).toDouble

            if (!questionLines.contains(qid)) {
              questionLines(qid) = new ArrayBuffer[String]
            }
            questionLines(qid).append(line)
          }

          lineCount += 1
        }
      }

    // Step 2: Output separate files
    for (key <- questionLines.keySet) {
      val qid = key

      val filenameOut = path + "/" + key + ".tsv"
      println ("Writing " + filenameOut)

      val pw = new PrintWriter(filenameOut)
      pw.println(headerLine)
      for (line <- questionLines(key)) {
        pw.println(line)
      }
      pw.flush()
      pw.close()

    }

  }


  def main(args:Array[String]): Unit = {

    chunkRobertaOyvind("explregen/oyvind-roberta/", "roberta_cl_wtrank-try1-eval-dev-all.tsv")

  }
}