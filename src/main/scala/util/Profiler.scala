package util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A simple code profiler
  * Created by peter on 10/10/17.
  */
class Profiler {

}

object Profiler {
  val keyIdxLUT = mutable.Map[String, Int]()
  val executionTime = new ArrayBuffer[Long]
  val counts = new ArrayBuffer[Long]
  val startTimes = new ArrayBuffer[Long]

  def start(key:String) {

    // Handle new keys
    if (!keyIdxLUT.contains(key)) {
      val newIdx = executionTime.size
      keyIdxLUT += (key -> newIdx)

      executionTime.append( 0 )
      counts.append(0)
      startTimes.append(0)
    }

    // Note start time
    val idx = keyIdxLUT(key)
    startTimes(idx) = System.nanoTime()
  }


  def end(key:String) {
    val endTime = System.nanoTime()
    val idx = keyIdxLUT(key)

    val deltaTime = endTime - startTimes(idx)
    executionTime(idx) += deltaTime
    counts(idx) += 1
  }


  // Reset the information in the profiler (except for key names, which remain)
  def reset() {
    val size = keyIdxLUT.size
    for (i <- 0 until size) {
      executionTime(i) = 0
      counts(i) = 0
      startTimes(i) = 0
    }
  }


  // Display
  def getReportString(sorted:Boolean = false):String = {
    val os = new StringBuilder()

    os.append("------------------------------------------------------------------------------------------------------\n")
    os.append("Profiler Report\n")
    os.append("\n")

    os.append(" ".formatted("%30s") + "\t" + "totalTime".formatted("%15s") + "\t" + "counts".formatted("%10s") + "\t" + "avgTime".formatted("%15s") + "\n")



    for (keyIdxPair <- keyIdxLUT.toList.sortBy(_._1)) {
      val key = keyIdxPair._1
      val idx = keyIdxLUT(key)
      val totalTime = executionTime(idx)
      val totalCounts = counts(idx)
      val avgTime = executionTime(idx).toDouble / counts(idx).toDouble

      os.append(key.formatted("%30s") + "\t")
      os.append( unitHelper(totalTime).formatted("%15s") + "\t")
      os.append( totalCounts.formatted("%10s") + "\t")
      os.append( unitHelper(avgTime.toLong).formatted("%15s"))
      os.append("\n")
    }

    os.append("\n")
    os.append("------------------------------------------------------------------------------------------------------\n")

    // Return
    os.toString()
  }


  def unitHelper(num:Long):String = {
    if (num < 1000) {
      return num.toDouble.formatted("%3.3f") + " nsec"
    } else if (num < 1000000) {
      return (num.toDouble / 1000).formatted("%3.3f") + " usec"
    } else if (num < 1000000000) {
      return (num.toDouble / 1000000).formatted("%3.3f") + " msec"
    } else {
      return (num.toDouble / 1000000000).formatted("%3.3f") + " sec"
    }
  }

}
