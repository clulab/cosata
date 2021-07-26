package inferenceengineexperiments.preprocessing

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object SplitQuestionsToSmallerFiles {

  def loadQuestionsFile(filename:String):(String, Array[String]) = {
    var header:String = ""
    val lines = new ArrayBuffer[String]

    var lineCount: Int = 0
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      if (lineCount == 0) {
        header = line
      } else {
        lines.append(line)
      }
      lineCount += 1
    }

    // Return
    (header, lines.toArray)
  }

  def main(args:Array[String]): Unit = {
    val filenameOutPrefix = "annotation/expl-tablestore-export-2020-02-17-123232/questions.all.nodup.filtered.chunk"
    val maxQuestionsPerFile:Int = 500

    val filenamesIn = new ArrayBuffer[String]
    filenamesIn.append("annotation/expl-tablestore-export-2020-02-17-123232/questions.train.tsv.nodup.filtered.tsv")
    filenamesIn.append("annotation/expl-tablestore-export-2020-02-17-123232/questions.dev.tsv.nodup.filtered.tsv")
    filenamesIn.append("annotation/expl-tablestore-export-2020-02-17-123232/questions.test.tsv.nodup.filtered.tsv")

    // Load
    var header:String = ""
    var lines = new ArrayBuffer[String]
    for (filenameIn <- filenamesIn) {
      val (header_, lines_) = loadQuestionsFile(filenameIn)
      header = header_
      lines.insertAll(lines.size, lines_)
    }


    // Create slices
    val slices = new ArrayBuffer[Array[String]]
    while (lines.length > 0) {
      if (lines.length > maxQuestionsPerFile) {
        val section = lines.slice(0, maxQuestionsPerFile)
        lines = lines.slice(maxQuestionsPerFile, lines.length)
        slices.append(section.toArray)
      } else {
        val section = lines.slice(0, lines.length)
        lines = new ArrayBuffer[String]
        slices.append(section.toArray)
      }
    }

    // Output
    for (i <- 0 until slices.length) {
      val filenameOut = filenameOutPrefix + i + ".tsv"
      println(" * Writing: " + filenameOut)

      val pw = new PrintWriter(filenameOutPrefix + i + ".tsv")
      pw.println(header)
      for (line <- slices(i)) {
        pw.println(line)
      }
      pw.flush()
      pw.println()
    }


  }

}
