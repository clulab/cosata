package inferenceengine.iml.constraint
import scala.util.control.Breaks._


/**
  * Iterator for all combinations/permutations of a set of maxValue.size numbers, where each number has some range ( 0 to maxValue(i)-1 )
  * This versions allows 'skipping' large sections of the iteration if the target algorithm is sure there are not any valid combinations
  * down particular iteration paths.  This essentially allows the iterator to be used for backtracking.
  * Created by peter on 2/9/20.
  */
class CombinationIteratorSkippable(val maxValues:Array[Int]) {
  private val numValues = maxValues.length
  private val out = Array.fill[Int](numValues)(0)
  private var isComplete:Boolean = false
  var count:Long = 0
  val size:Long = calculateSize()
  //private var lastIndexIncremented:Int = (numValues-1)
  private var finishedIterating:Boolean = false

  /*
  // Returns true if there are additional patterns to iterate through
  def hasNext():Boolean = {
    if (count < size) return true
    // Return
    false
  }
   */

  // Check to see if we've reached the end of the iterator.
  def isAtEnd():Boolean = {
    if (finishedIterating) return true

    for (i <- 0 until numValues) {
      if (out(i) != maxValues(i)-1) return false
    }

    // If we reach here, we're at the last element -- set finishedIterating to be true, while returning false (so the last element can be read, and the next call will return that there are no more left)
    finishedIterating = true
    // Return
    false
  }

  // Retrive the next pattern
  def next():Array[Int] = {
    if (count != 0) increment()
    count += 1
    out
  }

  // Skip over a large number of patterns starting from the last element that was incremented.
  def nextSkip(whichIdx:Int):Array[Int] = {
    if (count != 0) incrementSkip(whichIdx)
    out
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

  private def incrementSkip(whichIdx:Int): Unit = {
    //if (count >= size) return

    //## println ("lastIndexIncremented: " + lastIndexIncremented)
    //## println (" before: " + out.mkString(", "))

    // Check for case where we're trying to skip over at the end of the first row, which would increment over the max values.
    if ((whichIdx == 0) && (out(0) == maxValues(0)-1)) {
      for (i <- 0 until numValues) out(i) = maxValues(i)-1
      return
    }

    breakable {
      // Reset all values before
      for (i <- (numValues-1) until whichIdx by -1) {
        out(i) = 0
      }
      // Increment values starting at lastIndexIncremented
      for (i <- whichIdx to 0 by -1) {
        if ( out(i) < (maxValues(i)-1) ) {
          out(i) += 1
          break()
        } else {
          out(i) = 0
        }
      }
      // If we reach here, then there are no more patterns to iterate through
      //isComplete = true
      finishedIterating = true      //## TODO: Possibly a bug here, may skip over valid potential iterations?
    }

    //## println ("  after: " + out.mkString(", "))

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


object CombinationIteratorSkippable {

  // Example of use
  def main(args:Array[String]): Unit = {
    val maxValues = Array(5, 4, 6, 4)
    val iter = new CombinationIteratorSkippable( maxValues )

    println ("size: " + iter.size)

    var count:Int = 0

    while (!iter.isAtEnd()) {
      println ( count + "\t" + iter.next().toList )
      count += 1

      if (count % 16 == 0) {
        println ("nextSkip()")
        iter.nextSkip(3)
      }

    }

  }

}
