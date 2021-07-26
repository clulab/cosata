package util

import collection.mutable.ArrayBuffer
import java.io.PrintWriter
import edu.arizona.sista.struct.Counter
import Histogram.constWidthString

/**
 * Class to compute and display a histogram
 * User: peter
 * Date: 7/31/13
 */
class Histogram(var name:String, histBins:Array[Double] = Array(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.80, 0.9, 1.01)) {
  // Array(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.80, 0.85, 0.9, 0.95, 1.0)
  val histData = new Array[Int](histBins.size)                          // Counts for each bin
  val histNums = new Array[ArrayBuffer[String]](histBins.size)          // Tags for each bin (ie. the question numbers in that bin)
  var totalCounts:Double = 0.0

  // Constructor -- Initialize data structures
  for (i <- 0 until histBins.size) {
    histData(i) = 0
    histNums(i) = new ArrayBuffer[String]
  }

  /*
   * Add a datapoint to the historgram
   * score: Data to add (-1 to 1)
   * tag: An optional string associated with the datapoint (e.g. "Question #10") that will be displayed with the histogram
   */
  def addData(score:Double, tag:String) {
    // Histogram computation and binning
    for (j <- 0 until ((histBins.size) - 1)) {
      if ((score >= histBins(j)) && (score < histBins(j + 1))) {
        histData(j) += 1
        histNums(j).append(tag)
      }
    }
    totalCounts += 1
  }

  /*
   * Display the histogram
   */

  def toTSVString(delim:String = "\t"):String = {
    val os = new StringBuffer()

    os.append( name + " Histogram \n")
    // Bin labels
    for (j <- 0 until ((histBins.size) - 1)) {
      var binText = (histBins(j) * 100).formatted("%3.1f") + "% to " + (histBins(j + 1) * 100).formatted("%3.1f") + "%"
      os.append(binText + delim)
    }
    os.append("\n")

    // Bin values
    for (j <- 0 until ((histBins.size) - 1)) {
      os.append(histData(j) + delim)
    }
    os.append("\n")

    // Bin proportions
    for (j <- 0 until ((histBins.size) - 1)) {
      os.append((histData(j) / totalCounts).formatted("%3.3f") + delim)
    }
    os.append("\n")

    os.toString()
  }


  override def toString():String = {
    val os = new StringBuffer()

    os.append( name + " Histogram \n")
    var propSum:Double = 0.0

    for (j <- 0 until ((histBins.size) - 1)) {
      var binText = (histBins(j) * 100).formatted("%3.1f") + "% to " + (histBins(j + 1) * 100).formatted("%3.1f") + "%"
      binText = constWidthString(binText, 20)     // constant width formatting

      os.append(binText + " : " + histData(j) + "  \t")
      val oldPropSum = propSum
      if (totalCounts > 0) {
        os.append("(" + (histData(j) / totalCounts).formatted("%3.3f") + ")")
        propSum += (histData(j) / totalCounts)
      } else {
        os.append("(0.0%)")
      }

      os.append("\t [" + oldPropSum.formatted("%3.3f") + "]")
      os.append("\n")
      /*
      os.append(" {")
      val histQs = histNums(j)
      for (k <- 0 until histQs.size) {
        os.append(histQs(k) + ", ")
      }
      os.append("}\n")
      */
    }
    os.append("\n")

    os.toString()
  }

}


// Histogram class for histograms whose bins are discrete strings rather than numeric values
class HistogramString(var name:String) {
  val data = new Counter[String]
  var datums:Double = 0.0

  def addData(bin:String) {
    data.incrementCount(bin)
    datums += 1
  }

  override def toString():String = {
    val os = new StringBuilder()

    os.append (name + " Histogram" + "\n")
    val keys = data.keySet.toArray
    val sortedKeys = keys.sorted

    for (key <- sortedKeys) {
      var count = data.getCount(key)
      os.append(constWidthString(key, 20) + "\t" + count + " (" + (count / datums).formatted("%3.3f") + ")\n" )
    }

    os.append("\n")

    os.toString()
  }

}

object Histogram {

  def constWidthString(text:String, width:Int):String = {
    var out = text
    for (a <- 0 until (width - out.length)) out += " " // Constant width formatting
    out
  }
}