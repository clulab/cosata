package inferenceengine.iml.runtime

/*
 * A quick utility object to keep track of program execution time, with the ability to periodically check a timer to see if a time limit has exceeded.
 */

object TimeLimit {
  var timeLimit:Long = 0
  var timeStart:Long = 0
  var hasTimeLimit:Boolean = false

  def setTimeLimitMSec(in:Long): Unit = {
    timeLimit = in
    hasTimeLimit = true
  }

  def setTimeStart(): Unit = {
    timeStart = System.currentTimeMillis()
  }

  def getDeltaTime():Long = {
    val curTime = System.currentTimeMillis()
    val delta = curTime - timeStart
    // Return
    delta
  }

  def isTimeElapsed():Boolean = {
    if (timeLimit == 0) return false
    if (getDeltaTime() >= timeLimit) return true
    // Default return
    false
  }

}
