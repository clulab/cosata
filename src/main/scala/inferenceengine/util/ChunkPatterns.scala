package inferenceengine.util

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer


object ChunkPatterns {


  def readPatterns(filename:String):Array[Array[String]] = {
    val out = new ArrayBuffer[Array[String]]()

    val buffer = new ArrayBuffer[String]
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      buffer.append(line)
      if (line.length <= 0) {
        out.append( buffer.toArray )
        buffer.clear()
      }
    }

    println (" * Loaded " + out.length + " inference patterns. ")
    return out.toArray
  }


  def exportChunks(patternsIn:Array[Array[String]], chunkSize:Int, filenameOutPrefix:String): Unit = {
    val numPatterns = patternsIn.length

    var chunkNum:Int = 1
    // Create chunk
    for (i <- 0 until (numPatterns-chunkSize) by chunkSize) {

      val chunk = patternsIn.slice(i, i+chunkSize)

      val filenameOut = filenameOutPrefix + "-chunkSize" + chunkSize + "-chunk" + chunkNum + ".iml"
      println (" * Writing " + filenameOut)

      val pw = new PrintWriter(filenameOut)
      for (chunkPattern <- chunk) {
        for (line <- chunkPattern) {
          pw.println(line)
        }
      }
      pw.close()

      chunkNum += 1
    }


  }


  def main(args:Array[String]): Unit = {

    val pathIn = "/home/peter/github/worldtree-api-python/"
    //val filenameIn = "patternsOut-auto-fromq2207-MODE_WT21-minFreq-2-numEdges-1-np1740.iml"
    val filenameIn = "patternsOut-auto-fromq2207-MODE_TEACHER-minFreq-5-numEdges-2-np285875.iml"
    val pathOut = "/home/peter/github/worldtree-api-python/iml-chunks/"

    val patterns = readPatterns(pathIn + filenameIn)
    exportChunks(patterns, chunkSize = 50, pathOut + filenameIn)

  }

}
