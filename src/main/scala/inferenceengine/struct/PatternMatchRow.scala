package inferenceengine.struct

import edu.arizona.sista.struct.Lexicon
import explanationgraph.TableRow
import util.LexiconUtil

import scala.util.control.Breaks._

/**
  * Storage class representing one table row and all the cell matches it has
  * Created by user on 6/28/18.
  */
class PatternMatchRowFlex(val row:TableRow, val cellMatches:Array[Array[FlexiblePatternMatchCell]], val generatedFromStaticUUID:Boolean) {

  // cellMatches: Outer array is per column index, inner array is for a given column index, what are the potential matches
  def getNumRelevantColumns():Int = {
    cellMatches.length
  }

  def getNumPotentialFlexMatches(relColIdx:Int):Int = {
    cellMatches(relColIdx).length
  }

  def getPotentialFlexMatches(relColIdx:Int):Array[FlexiblePatternMatchCell] = {
    cellMatches(relColIdx)
  }

  def getMatches():Array[Array[FlexiblePatternMatchCell]] = {
    cellMatches
  }

  def getRow():TableRow = {
    row
  }

  /*
   * String methods
   */
  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append( row.toStringSentWithUID() + "\n")
    for (matchIdx <- 0 until cellMatches.length) {
      for (flexCellIdx <- 0 until cellMatches(matchIdx).length) {
        val flexCell = cellMatches(matchIdx)(flexCellIdx)
        os.append("matchIdx: " + matchIdx + "\t flexCellIdx: " + flexCellIdx + "\t" + flexCell.toString(lexicon) + "\n")
      }
    }

    os.toString()
  }

}

class PatternMatchRow(val row:TableRow, val cellMatches:Array[FlexibleSpanMatchCollection], val generatedFromStaticUUID:Boolean) {

  def numMatches:Int = {
    cellMatches.length
  }

  def getRow():TableRow = {
    row
  }

  def getRelaxedCount():Int = {
    var sum:Int = 0
    for (m <- cellMatches) {
      sum += m.relaxedCount
    }
    //cellMatches.foreach(sum += _.relaxedCount)
    sum
  }

  /*
   * Cloning
   */
  override def clone():PatternMatchRow = {
    // Shallow copy, since these are largely used staticly
    new PatternMatchRow(row, cellMatches, generatedFromStaticUUID)
  }

  /*
   * Hashcode
   */

  /*
  // OLD/DEPRICATED
  def getRowHashCode():Int = {
    // Knuth hashing
    var hash = 0
    for (ch <- row.uid.toCharArray) {
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    }
    // Return
    hash % 1000000
  }
  */



  // Note: Two different methods of hashcode generation are used -- one for static rows, which is based on the UUID,
  //       and one based on actual row content, which is used for automatically generated rows (since the UUID of those rows can
  //       change within and across simulations, but the content should be the same unless the inference pattern generating
  //       that content changes).
  def getRowHashCode():Int = {
    // Check for hashcode format -- static or dynamic

    // Static UUID: Check for XXXX-XXXX-XXXX-XXXX format.
    if ((row.uid.length >= 19) && (row.uid.charAt(4) == '-' && row.uid.charAt(9) == '-' && row.uid.charAt(14) == '-')) {
      //println ("Static hashcode: " + row.uid)
      getRowHashCodeUUID()
    } else {
      //println ("Dynamic hashcode: " + row.uid)
      // Assume non-static UID
      getRowHashCodeContent()
    }
  }


  // Note: This method of hashcode generation should only be used for static rows, not automatic rows.
  private def getRowHashCodeUUID():Int = {
    // Return
    knuthHashing(row.uid)
  }

  // Note: This method of hashcode generation should only be used for automatically generated rows, with automatically
  // generated and changing UUIDs.
  private def getRowHashCodeContent():Int = {
    // Step 1: Generate a string representing the text of the row, but only containing content (i.e. non-fill, non-skip) columns.
    val rowContentStr = row.toStringDataColTextOnly().toLowerCase

    // Step 2: Generate hashcode
    // Return
    knuthHashing(rowContentStr)
  }

  private def knuthHashing(in:String):Int = {
    // Knuth hashing
    var hash = 0
    for (ch <- in.toCharArray) {
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    }
    // Return
    hash % 1000000
  }


  /*
   * String methods
   */

  override def toString():String = {
    val os = new StringBuilder

    os.append("Row: " + row.toStringSentWithUID() + "\n")
    os.append("Cell Matches: \n")

    for (i <- 0 until cellMatches.length) {
      os.append("\t" + "CellMatch " + i + " \t" + cellMatches(i) + "\n")
    }

    // Return
    os.toString()
  }


  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append(row.toStringSentWithUID() + "\n")
    os.append("Cell Matches: \n")

    for (i <- 0 until cellMatches.length) {
      os.append("\t" + "CellMatch " + i + " \t" + cellMatches(i).toString(lexicon) + "\n")
    }

    // Return
    os.toString()
  }


  def toStringMinimal():String = {
    val os = new StringBuilder

    os.append(row.toStringSentWithUID())

    // Return
    os.toString()
  }


  // Places highlighting text (e.g. <>) around variable text in a row string, to make them easier to visually parse
  // TODO: Currently buggy
  def toStringVariableHighlights(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder
    val debugOutput:Boolean = false

    if (debugOutput)  println ("istoStringVariableHighlights(): Started on row: " + row.toStringSentWithUID() + ")")

    val validColumns = row.table.getDataAndFillColumns()
    //## println ("validColumns: " + validColumns.mkString(", "))
    for (colIdx <- validColumns) {
      val colName = row.table.getSanitizedColumnName(colIdx)
      //os.append(colIdx + "/" + colName + " ")

      var foundMatch:Boolean = false
      for (i <- 0 until cellMatches.length) {
        if (cellMatches(i).colIdx == colIdx) {
          foundMatch = true
          val elems = cellMatches(i).matches
          if (cellMatches(i).relaxedCount > 0) os.append("~")

          for (elem <- elems) {
            if (elem.isLexicalPattern) {
              os.append("\"")
              os.append(LexiconUtil.lexiconIdxsToStr(elem.spanWords, lexicon))
              os.append("\" ")
            } else if (elem.isVariable) {
              os.append("&lt;")
              os.append(LexiconUtil.lexiconIdxsToStr(elem.spanWords, lexicon))
              os.append("&gt; ")
            } else {
              //os.append("NLOV")
              os.append(" ")
            }
          }

          if (cellMatches(i).relaxedCount > 0) os.append("~")
        }
      }

      if (!foundMatch) {
        // If we reach here, there were no matches for this column -- output the raw cell text
        for (altIdx <- 0 until row.getCellNumAlternatives(colIdx)) {
          os.append( LexiconUtil.lexiconIdxsToStr(row.getCellWordsAlt(colIdx, altIdx), lexicon) )
          if (altIdx < (row.getCellNumAlternatives(colIdx)-1)) os.append(" ; ")
        }
      }

      os.append(" ")
    }


    //## Debug -- also append UUID
    os.append("(" + row.uid + ")")

    // Return
    os.toString()
  }


}
