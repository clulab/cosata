package util

import edu.arizona.sista.struct.Lexicon

import scala.collection.mutable.ArrayBuffer

/**
  * Created by user on 7/25/18.
  */

object LexiconUtil {

  def strToLexiconIdxs(in:String, lexicon:Lexicon[String]):Array[Int] = {
    val out = new ArrayBuffer[Int]

    val split = in.split(" ")
    for (elem <- split) {
      val trimmed = elem.trim()
      if (trimmed.length > 0) {
        out.append( lexicon.add(trimmed) )
      }
    }

    // Return
    out.toArray
  }


  def lexiconIdxsToStr(in:Array[Int], lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (idx <- in) {
      os.append(lexicon.get(idx) + " ")
    }

    // Return
    os.toString.trim()
  }
}
