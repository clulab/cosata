package util

import edu.arizona.sista.struct.Counter

/**
  * Load/Save routines for Counters
  * Created by peter on 2/7/19.
  */

object CounterIO {
  // Note, these do not currently load/save the default value

  def saveCounterToString(in:Counter[String], delim:String = "\t"):String = {
    val os = new StringBuilder

    for (key <- in.keySet) {
      os.append(key + delim + in.getCount(key) + delim)
    }

    // Return
    os.toString()
  }

  def loadCounterFromString(strIn:String, delim:String = "\t"):Counter[String] = {
    val out = new Counter[String]
    val elements = strIn.split(delim)

    //println ("strIn: '" + strIn + "'")
    // Check for empty case
    if (strIn.length == 0) return out

    for (i <- 0 until elements.length by 2) {
      //println ("i: " + i)
      //println (elements(i) + "\t" + elements(i+1))
      out.setCount(elements(i), elements(i+1).toDouble)   // key, value
    }

    // Return
    out
  }


}
