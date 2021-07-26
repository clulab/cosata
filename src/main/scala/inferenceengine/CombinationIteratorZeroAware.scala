package inferenceengine

import edu.arizona.sista.utils.Profiler

import scala.util.control.Breaks._

/**
  * Iterator for all combinations/permutations of a set of maxValue.size numbers, where each number has some range ( 0 to maxValue(i)-1 )
  * This iterator is zero-aware, and any elements with maxValue of 0 will be set to -1 and not iterated.
  * Created by peter on 10/14/17.
  */
class CombinationIteratorZeroAware(val maxValues:Array[Int]) {
  private val numValues = maxValues.length
  private val out = Array.fill[Int](numValues)(0)
  private var isComplete:Boolean = false
  var count:Long = 0
  val size:Long = calculateSize()

  // Any elements with a maxValue of 0 should be set to -1, to indicate that they are invalid
  for (i <- 0 until maxValues.length) {
    if (maxValues(i) == 0) out(i) = -1
  }

  // Returns true if there are additional patterns to iterate through
  def hasNext():Boolean = {
    if (count < size) return true
    // Return
    false
  }

  // Retrive the next pattern
  def next():Array[Int] = {
    if (count != 0) increment()
    count += 1
    out
  }

  // Increment the pattern
  private def increment(): Unit = {
    if (count >= size) return

    breakable {
      // Slow case
      for (i <- (numValues-1) to 0 by -1) {
        if ( out(i) < (maxValues(i)-1) ) {
          out(i) += 1
          break()
        } else {
          if (out(i) != -1) {
            out(i) = 0
          }
        }
      }
      // If we reach here, then there are no more patterns to iterate through
      //isComplete = true
    }
  }


  private def calculateSize():Long = {
    var sum:Long = 1
    for (i <- 0 until numValues) {
      if (maxValues(i) != 0) {
        sum *= maxValues(i)
      }
    }
    // Return
    sum
  }

}


object CombinationIteratorZeroAware {

  // Example of use
  def main(args:Array[String]): Unit = {
    val maxValues = Array(10000, 1000)
    //val maxValues = Array(5, 2, 3)
    //val maxValues = Array(5, 0, 3, 0, 0, 2)
    val iter = new CombinationIteratorZeroAware( maxValues )

    println ("size: " + iter.size)

    val startTime = System.nanoTime()
    var count:Int = 0
    while (iter.hasNext()) {
      //println ( iter.next().toList )
      iter.next()
      count += 1
    }
    val endTime = System.nanoTime()

    println ((endTime - startTime) / 1000000 + " msec")
    println ("Count: " + count)
    println ("Complete")
  }

}
