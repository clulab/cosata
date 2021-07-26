package explanationgraph

import scala.util.control.Breaks.{break, breakable}

/**
  * Iterator for all combinations/permutations of a set of indices, where each indice has a specific array of valid values (e.g. Array(1, 3, 5, 7, 8, 9, 11) ).
  * Created by peter on 7/31/18.
  */
class CombinationIteratorCustom(val validIterations:Array[Array[Int]]) {
  val maxValues = new Array[Int](validIterations.length)
  for (i <- 0 until validIterations.length) {
    maxValues(i) = validIterations(i).length
  }
  private val numValues = maxValues.length
  private val out = Array.fill[Int](numValues)(0)
  private var isComplete:Boolean = false
  var count:Long = 0
  val size:Long = calculateSize()

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
    convertCountToPattern( out )
  }

  // Increment the pattern
  private def increment(): Unit = {
    if (count >= size) return

    breakable {
      for (i <- (numValues-1) to 0 by -1) {
        if ( out(i) < (maxValues(i)-1) ) {
          out(i) += 1
          break()
        } else {
          out(i) = 0
        }
      }
      // If we reach here, then there are no more patterns to iterate through
      //isComplete = true
    }
  }

  private def convertCountToPattern(count:Array[Int]):Array[Int] = {
    val out = new Array[Int](count.length)
    for (i <- 0 until count.length) {
      out(i) = validIterations(i)(count(i))
    }
    // Return
    out
  }

  private def calculateSize():Long = {
    var sum:Long = 1
    for (i <- 0 until numValues) {
      sum *= maxValues(i)
    }
    // Return
    sum
  }

}


object CombinationIteratorCustom {

  // Example of use
  def main(args:Array[String]): Unit = {
    val validIterations = Array( Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9, 10) )
    val iter = new CombinationIteratorCustom( validIterations )

    println ("size: " + iter.size)

    while (iter.hasNext()) {
      println ( iter.next().toList )
    }

  }

}
