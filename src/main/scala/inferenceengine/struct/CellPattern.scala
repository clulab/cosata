package inferenceengine.struct

import explanationgraph.{CombinationIterator, Table, TableRow}
import TableRow.{MODE_ANY, MODE_LEMMA, MODE_TLEMMA, MODE_TWORD, MODE_WORD}
import edu.arizona.sista.struct.Lexicon
import inferenceengine.CombinationIteratorZeroAware
import inferenceengine.iml.constraint.VariableList
import org.slf4j.LoggerFactory
import util.LexiconUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Storage class for a pattern in a cell, which can consist of variable(s), lexical pattern(s), or both.
  * e.g. matching "device", "<x> powered device", or "<x> powered <y>"
  * Created by peter on 6/27/18.
  */
class CellPattern(val columnName:String, val patternOptions:Array[Array[PatternElem]], numVariables:Array[Int], numLexicalPatterns:Array[Int]) {
  // Constructor

  // Step 0: Validity checks
  /*
  // Note (PJ): Removed this, since multiple lexical constraints adjacent to each other do work (and may be entered by the user), they just use the (potentially slower) 'mixed' solver
  if ((isAllLexical) && (numLexicalPatterns > 1)) {
    throw new RuntimeException("CellPattern: ERROR: Multiple lexical patterns with zero variable patterns.  These should be merged into a single lexical pattern. \n" + toString())
  }
  */
  /*
  // Note (PJ): Working to implement patterns where this is allowed
  if ((isAllVariable) && (numVariables > 1)) {
    throw new RuntimeException("CellPattern: ERROR: Multiple variable patterns with zero lexical patterns.  These should be merged into a single variable. \n" + toString())
  }
  for (i <- 0 until elems.size-1) {
    if (elems(i).isVariable && elems(i+1).isVariable) {
      throw new RuntimeException("CellPattern: ERROR: Multiple variable patterns adjacent to each other (" + elems(i).getVariableName + ", " + elems(i+1).getVariableName + ").  Not currently possible to disambiguate.  \n" + toString())
    }
  }
  */
  // TODO: Check for multiple adjacent lexical patterns or variables.  These are currently unsupported, since they would be difficult to disambiguate.



  if (getNumOptions() == 0) {
    throw new RuntimeException("CellPattern: ERROR: Empty pattern.")
  }
  for (optionIdx <- 0 until getNumOptions()) {
    if (getNumElems(optionIdx) == 0) {
      throw new RuntimeException("CellPattern: ERROR: Empty option.")
    }
  }

  // Step 1: Precompute variableNameLUT (the profiler shows a lot of time is spend on this -- so this provides a speed boost.)
  // Note: could also change this to using a lexicon instead of String for a further speed boost.
  // TODO: Technically this should be a Set, because there could be patterns that reuse the same variable (e.g. <X> + " - " + <X>)
  val variableNameLUT = new Array[mutable.HashMap[String, Int]](patternOptions.length)

  for (optionIdx <- 0 until patternOptions.length) {
    variableNameLUT(optionIdx) = new mutable.HashMap[String, Int]()
    val elemsOption = patternOptions(optionIdx)

    for (i <- 0 until elemsOption.length) {
      if (elemsOption(i).isVariable) {
        variableNameLUT(optionIdx) += (elemsOption(i).getVariableName -> i)
      }
    }
  }


  /*
   * General
   */
  def getNumOptions():Int = {
    patternOptions.length
  }

  def getNumElems(optionIdx:Int):Int = {
    patternOptions(optionIdx).length
  }

  def getElems(optionIdx:Int):Array[PatternElem] = {
    patternOptions(optionIdx)
  }

  def getElem(optionIdx:Int, elemIdx:Int):PatternElem = {
    patternOptions(optionIdx)(elemIdx)
  }


  /*
   * Lexical patterns
   */
  def isAllLexical(optionIdx:Int):Boolean = {
    if ((numLexicalPatterns(optionIdx) > 0) && (numVariables(optionIdx) == 0)) return true
    // Default
    false
  }

  def getLexicalPattern(optionIdx:Int):Array[Int] = {
    if (isAllLexical(optionIdx)) return patternOptions(optionIdx)(0).getLexicalPattern
    throw new RuntimeException("getLexicalPattern: ERROR: Pattern is not entirely lexical.")
    // Default
    Array.empty[Int]
  }


  /*
   * Variables
   */

  def isAllVariable(optionIdx:Int):Boolean = {
    if ((numVariables(optionIdx) > 0) && (numLexicalPatterns(optionIdx) == 0)) return true
    // Default
    false
  }

  def getAllVariables(optionIdx:Int):Set[String] = {
    val out = mutable.Set[String]()
    for (elem <- patternOptions(optionIdx)) {
      if (elem.isVariable) {
        out += elem.getVariableName
      }
    }
    // Return
    out.toSet
  }

  def getVariableNameIndex(name:String, optionIdx:Int):Int = {
    variableNameLUT(optionIdx).getOrElse(name, -1)
  }

  def getVariableNameElem(name:String, optionIdx:Int):Option[PatternElem] = {
    val idx = getVariableNameIndex(name, optionIdx)
    if (idx == -1) return None
    Some(patternOptions(optionIdx)(idx))
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("[colName: " + columnName + "\t")
    if (getNumOptions() == 1) {
      os.append(" elements: " + getElems(0).mkString(", "))
    } else {
      os.append(" Options(ORs): " + getNumOptions())
      for (optionIdx <- 0 until getNumOptions()) {
        os.append(" (OPT" + optionIdx + ", ")
        os.append(" elements: " + getElems(optionIdx).mkString(", "))
        os.append(") ")
      }

    }
    os.append("]")

    os.toString
  }


}


object CellPattern {
  val logger = LoggerFactory.getLogger(classOf[CellPattern])


  // Generator
  def mkCellPattern(columnName:String, patternOptions:Array[Array[PatternElem]]):CellPattern = {
    // Count the number of variables and lexical patterns in this cell
    val numVariables = Array.fill[Int](patternOptions.length)(0)
    val numLexicalPatterns = Array.fill[Int](patternOptions.length)(0)
    for (optionIdx <- 0 until patternOptions.length) {
      val patternOption = patternOptions(optionIdx)
      for (element <- patternOption) {
        if (element.isVariable) numVariables(optionIdx) += 1
        if (element.isLexicalPattern) numLexicalPatterns(optionIdx) += 1
      }
    }

    new CellPattern(columnName = columnName, patternOptions = patternOptions, numVariables = numVariables, numLexicalPatterns = numLexicalPatterns)
  }




  /*
   * Checking single rows
   */
  /*
  // Check to see if a given row meets the constraints of a given ColumnValue
  def meetsContraint(row:TableRow, cellPat:CellPattern):Boolean = {
    // Step 1: Check if column exists
    val colIdx = row.table.getColumnIdx( colVal.columnName )

    // Return false if column is not found
    if (colIdx < 0) return false

    // Step 2: Check if column has the same value on at least one of the possibleValues (OR)
    meetsConstraintHelper(row, colIdx, colVal)

  }
  */

  /*
  // This is the old, string-based (i.e. non-lexicon based version)
  // depricated
  private def matchConstraintsOld(row:TableRow, colIdx:Int, cellPat:CellPattern): Option[PatternMatchCell] = {
    // Step 0: Fetch cell text
    val cellText = row.getCellText(colIdx)
    val locations = new Array[(Int, Int)](cellPat.getNumElems)

    // Step 1: Check for easy cases
    // Step 1A: Entirely lexical pattern
    if (cellPat.isAllLexical) {
      if (cellText == cellPat.getLexicalPattern) {
        // TODO: Match
        locations(0) = (0, cellText.length)
        return Some(new PatternMatchCell(cellPat, cellText, locations))
      } else {
        // TODO: Failure to match.  Return failure.
        return None
      }
    }

    // Step 1B: Entirely variable pattern
    if (cellPat.isAllVariable) {
      // TODO: Assign to variable
      locations(0) = (0, cellText.length)
      return Some(new PatternMatchCell(cellPat, cellText, locations))
    }

    // Step 2: Mixed case: Pattern contains one or more variables, and one or more lexical patterns.

    // Initialize array
    for (i <- 0 until locations.length) {
      locations(i) = (-1, -1)
    }

    // Step 2A: Solve constraints on lexical patterns
    var lastIdx:Int = 0
    for (i <- 0 until cellPat.elems.length) {
      if (cellPat.getElem(i).isLexicalPattern) {
        val lexPattern = cellPat.getElem(i).getLexicalPattern
        val patternLength = lexPattern.length

        println ("i: " + i)
        println ("cellText: " + cellText)
        println ("lexPattern: " + lexPattern)


        breakable {
          while (true) {
            // Check if we can make the comparison
            println ("lastIdx+patternLength: " + (lastIdx+patternLength) )
            println ("cellText.length: " + cellText.length)
            if ((lastIdx + patternLength) > cellText.length) {
              // TODO: Failure to match. Pattern we're looking for is too long to hold in the unsearched remainder of the cell.  Return failure.
              println ("Exceeds limits")
              return None
            }

            // Make the comparison
            //logger.debug ("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
            //logger.info ("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
            println ("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
            if (cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase == lexPattern) {
              // TODO: Matched this section
              //logger.info("Match")
              println ("Match")
              locations(i) = (lastIdx, lastIdx + patternLength)
              break()
            }

            lastIdx += 1
          }
        }

      }
    }

    // If we've reached this section, then all lexical pattern constraints have been met.  Solve variable constraints.
    // Step 2B: Assign remaining string to variables
    for (i <- 0 until cellPat.elems.length) {
      if (cellPat.getElem(i).isVariable) {
        val variableName = cellPat.getElem(i).getVariableName

        if (i == 0) {
          // First element
          locations(i) = (0, locations(i+1)._1)
        } else if (i == cellPat.elems.length-1) {
          // Last element
          locations(i) = (locations(i-1)._2, cellText.length)
        } else {
          // Any middle element
          locations(i) = (locations(i-1)._2, locations(i+1)._1)
        }

        // TODO: Check for non-zero length
        // TODO: Check for non-zero content

      }
    }

    // TODO: Make variable assignments
    val out = new PatternMatchCell(cellPat, cellText, locations)

    // Return
    Some(out)
  }
  */


  /*
  // This is the old, string-based (i.e. non-lexicon based version)
  // depricated -- use matchConstraintsOmni()
  private def matchConstraints(row:TableRow, colIdx:Int, cellPat:CellPattern, lexicon:Lexicon[String], onlyContentTags:Boolean = true, allowRelaxedConstraints:Boolean = false): Option[PatternMatchCell] = {
    val debugOutput:Boolean = false
    if (debugOutput) println ("\n* matchConstraints: started...")

    // Step 0: Fetch cell text
    val cellText = row.getCellText(colIdx)

    // Step 1: Check for easy cases
    // Step 1A: Entirely lexical pattern
    if (cellPat.isAllLexical) {
      val matchingAlternatives = row.findCellMatchesStrAlts(colIdx, cellPat.getLexicalPattern, MODE_ANY, onlyContentTags = onlyContentTags)

      if (debugOutput) {
        println("Cell Pattern is entirely lexical")
        println("ColIdx: " + colIdx)
        println("Pattern: " + LexiconUtil.lexiconIdxsToStr(cellPat.getLexicalPattern(), lexicon))
        println("Row contents: " + row.cellLemmas(colIdx)(0).mkString(", ") + " (" + LexiconUtil.lexiconIdxsToStr(row.cellLemmas(colIdx)(0), lexicon) + ")")
        println("Matching alternatives: " + matchingAlternatives.length)
      }

      if (matchingAlternatives.length > 0) {
        // TODO: MatchmatchedConstraints
        val cellAlternatives = new ArrayBuffer[CellAlternative]
        for (altIdx <- 0 until matchingAlternatives.length) {
          val locations = new Array[(Int, Int)](cellPat.getNumElems)
          val altCellIdx = matchingAlternatives(altIdx)
          //val cellWords = row.getCellWordsAlt(colIdx, altCellIdx)
          //val cellWords = row.getCellTLemmasAlt(colIdx, altCellIdx)     // Note: changed variable matching to matching taggedLemmas instead of words
          val cellWords = row.getCellLemmasAlt(colIdx, altCellIdx, onlyContentTags)     // Note: changed variable matching to matching Lemmas instead of words
          if (debugOutput) println ("CellWords: " + LexiconUtil.lexiconIdxsToStr(cellWords, lexicon) )
          locations(0) = (0, cellWords.length)
          cellAlternatives.append( CellAlternative.mkCellAlternative(cellPat, altIdx, cellWords, locations, lexicon) )
        }

        if (cellAlternatives.length > 0) {
          //println ("For Pattern: " + cellPat.toString())
          //println ("PM--> " + new PatternMatchCell(cellPat, cellAlternatives.toArray))   //###
          return Some(new PatternMatchCell(cellPat, cellAlternatives.toArray))

        } else {
          //return None
        }



      } else {
        // TODO: Failure to match to strict constraints.  Attempt to match using relaxed constriants, if enabled.

        if (cellPat.relaxable) {
          if (debugOutput) println("Checking relaxed constraints")
          // Step N: Hard matching did not find matches -- now check relaxed (set intersection) matching
          val matchingRelaxedAlternatives = row.findCellMatchesStrRelaxedAlts(colIdx, cellPat.getLexicalPattern, MODE_ANY, onlyContentTags = onlyContentTags)
          if (matchingRelaxedAlternatives.length > 0) {
            val cellAlternativesRelaxed = new ArrayBuffer[CellAlternative]
            for (altIdx <- 0 until matchingRelaxedAlternatives.length) {
              val locations = new Array[(Int, Int)](cellPat.getNumElems)
              val altCellIdx = matchingRelaxedAlternatives(altIdx)
              val cellWords = row.getCellLemmasAlt(colIdx, altCellIdx, onlyContentTags) // Note: changed variable matching to matching Lemmas instead of words
              if (debugOutput) println("CellWords: " + LexiconUtil.lexiconIdxsToStr(cellWords, lexicon))
              locations(0) = (0, cellWords.length)
              cellAlternativesRelaxed.append(CellAlternative.mkCellAlternative(cellPat, altIdx, cellWords, locations, lexicon))
            }

            if (cellAlternativesRelaxed.length > 0) {
              if (debugOutput) println("FOUND RELAXED CONSTRAINTS")
              return Some(new PatternMatchCell(cellPat, cellAlternativesRelaxed.toArray, relaxed = 1))
            } else {
              // No Matches
              return None
            }
          }
        } else {
          // Relaxable constraints not enabled -- return no matches.
          return None
        }

        // return None
      }
    }


    // Step 1B: Entirely variable pattern
    if (cellPat.isAllVariable) {

      if (debugOutput) {
        println("Cell Pattern is entirely variable")
        println("ColIdx: " + colIdx)
        println("Row contents: " + row.cellLemmas(colIdx)(0).mkString(", ") + " (" + LexiconUtil.lexiconIdxsToStr(row.cellLemmas(colIdx)(0), lexicon) + ")")
        //println("Matching alternatives: " + matchingAlternatives.length)
      }

      if (cellPat.relaxable) {
        //## println ("WARNING: Relaxable all-variable expressions are currently not implemented.  Using strict interpretation only. ")
      }


      // TODO: Assign to variable
      val cellAlternatives = new ArrayBuffer[CellAlternative]
      //## println ("row.getCellWords(colIdx).length: " + row.getCellWords(colIdx).length)
      for (altIdx <- 0 until row.getCellNumAlternatives(colIdx)) {
        val locations = new Array[(Int, Int)](cellPat.getNumElems)
        //val cellWords = row.getCellWordsAlt(colIdx, altIdx)
        //val cellWords = row.getCellTLemmasAlt(colIdx, altIdx)     // Note: changed variable matching to matching taggedLemmas instead of words
        val cellWords = row.getCellLemmasAlt(colIdx, altIdx, onlyContentTags)     // Note: changed variable matching to matching Lemmas instead of words
        locations(0) = (0, cellWords.length)
        cellAlternatives.append( CellAlternative.mkCellAlternative(cellPat, altIdx, cellWords, locations, lexicon) )
      }

      if (cellAlternatives.length > 0) {
        return Some(new PatternMatchCell(cellPat, cellAlternatives.toArray))
      } else {
        return None
      }
    }

    // Step 2: Mixed case: Pattern contains one or more variables, and one or more lexical patterns.

    if (debugOutput) {
      println("Cell Pattern is mixed (combination of lexical and variable constraints)")
      println("ColIdx: " + colIdx)
      //println("Pattern: " + LexiconUtil.lexiconIdxsToStr(cellPat.getLexicalPattern(), lexicon))
      println("Row contents: " + row.cellLemmas(colIdx)(0).mkString(", ") + " (" + LexiconUtil.lexiconIdxsToStr(row.cellLemmas(colIdx)(0), lexicon) + ")")
      //println("Matching alternatives: " + matchingAlternatives.length)
    }

    //## println (" * Mixed case")
    if (cellPat.relaxable) {
      //## println ("WARNING: Relaxable mixed expressions (variables combined with lexical patterns) are currently not implemented.  Using strict interpretation only. ")
    }

    val cellAlternatives = new ArrayBuffer[CellAlternative]
    for (altIdx <- 0 until row.getCellNumAlternatives(colIdx)) {
      //val cellWords = row.getCellWordsAlt(colIdx, altIdx)
      //val cellWords = row.getCellTLemmasAlt(colIdx, altIdx)     // Note: changed variable matching to matching taggedLemmas instead of words
      val cellWords = row.getCellLemmasAlt(colIdx, altIdx, onlyContentTags)     // Note: changed variable matching to matching Lemmas instead of words

      // Initialize locations array
      val locations = new Array[(Int, Int)](cellPat.getNumElems)
      for (i <- 0 until locations.length) {
        locations(i) = (-1, -1)
      }

      // Step 2A: Solve constraints on lexical patterns
      var lastIdx: Int = 0
      for (i <- 0 until cellPat.elems.length) {

        if (cellPat.getElem(i).isLexicalPattern) {
          val lexPattern = cellPat.getElem(i).getLexicalPattern
          val patternLength = lexPattern.length


          //## DEBUG
          if (debugOutput) {
            println("\tcellPat elem i: " + i)
            println("\tcellText: " + cellText)
            print("\tlexPattern: " + lexPattern.mkString(", ") + "   ")
            for (lexIdx <- lexPattern) print (row.lexicon.get(lexIdx) + " ")
            println ("")
          }


          breakable {
            while (true) {
              if (debugOutput) println ("\t\tlastIdx: " + lastIdx)

              // Check if we can make the comparison
              if ((lastIdx + lexPattern.length-1) > cellWords.length) {   //## PJ (1/24) added -1
              //if ((lastIdx + lexPattern.length) > cellWords.length) {   //## PJ (1/24) added -1
                // Failure to match. Pattern we're looking for is too long to hold in the unsearched remainder of the cell.  Return failure.
                if (debugOutput) println("\t\tExceeds limits")
                return None
              }

              if (debugOutput) println ("here0")
              // Try to match the lexical pattern to the cell text.
              val toIdx = row.cellMatchStrSpan(colIdx, altIdx, lastIdx, lexPattern, MODE_ANY, onlyContentTags = true)
              //##val toIdx = row.cellMatchStrSpan(colIdx, altIdx, lastIdx, lexPattern, MODE_LEMMA, onlyContentTags = true)
              if (debugOutput) println ("here1")
              if (toIdx > -1) {
                // TODO: Matched this section
                //logger.info("Match")
                if (debugOutput) println("\t\tMatch")
                locations(i) = (lastIdx, toIdx)
                if (debugOutput) println ("\t\tlocations(" + i + "): " + locations(i))
                break()
              }

              /*
              // Depricated: string-based method
              // Check if we can make the comparison
              println("lastIdx+patternLength: " + (lastIdx + patternLength))
              println("cellText.length: " + cellText.length)
              if ((lastIdx + patternLength) > cellText.length) {
                // TODO: Failure to match. Pattern we're looking for is too long to hold in the unsearched remainder of the cell.  Return failure.
                println("Exceeds limits")
                return None
              }

              // Make the comparison
              //logger.debug ("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
              //logger.info ("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
              println("Comparing '" + cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase + "' with '" + lexPattern + "'")
              if (cellText.substring(lastIdx, lastIdx + patternLength).toLowerCase == lexPattern) {
                // TODO: Matched this section
                //logger.info("Match")
                println("Match")
                locations(i) = (lastIdx, lastIdx + patternLength)
                break()
              }
              */

              //## println ("here2")
              lastIdx += 1
            }
          }

        }
      }

      //## println("\tcompleted iterating through cellPat elems")

      //## println ("\tSolving variable constraints")
      // If we've reached this section, then all lexical pattern constraints have been met.  Solve variable constraints.
      // Step 2B: Assign remaining string to variables
      for (i <- 0 until cellPat.elems.length) {
        //## println ("\t\tcellPat.elem: " + i + "    " + cellPat.getElem(i).toString() )
        if (cellPat.getElem(i).isVariable) {
          //## println ("\t\tisVariable: true")
          val variableName = cellPat.getElem(i).getVariableName
          //## println ("\t\tvariableName: " + variableName)

          if (i == 0) {
            // First element
            locations(i) = (0, locations(i + 1)._1)
          } else if (i == cellPat.elems.length - 1) {
            // Last element
            locations(i) = (locations(i - 1)._2, cellText.length)
          } else {
            // Any middle element
            locations(i) = (locations(i - 1)._2, locations(i + 1)._1)
          }

          //## println ("\t\tlocations(" + i + "): " + locations(i))

          // TODO: Check for non-zero length
          // TODO: Check for non-zero content

        }
      }

      // Check that each element has a valid location
      //## println ("\tChecking that all elements have been matched to locations: ")
      breakable {
        // Step 1: Check that all pattern elements have been matched to locations
        for (i <- 0 until cellPat.getNumElems) {
          // Check for a location that hasn't been found (i.e. the first index is -1)
          if (locations(i)._1 == -1) {
            //## println("false -- at least one location not found")
            break()
          }
          // Check for a zero-length location (the first and second indices being the same)
          if (locations(i)._1 == locations(i)._2) {
            //## println ("false -- zero-length location")
            break()
          }
        }

        // Step 2: Check that there are no leftover cellWords that haven't been matched.
        val lastLocationIdx = locations.last._2
        if (lastLocationIdx < cellWords.length) {
          //## println ("false -- leftover cellWords")
          break()
        }

        //## println ("true")

        cellAlternatives.append( CellAlternative.mkCellAlternative(cellPat, altIdx, cellWords, locations, lexicon) )
      }

    }
    //## println ("\tcompleted iterating through alternates")

    // TODO: Make variable assignments

    // Return
    if (cellAlternatives.length > 0) {
      val out = new PatternMatchCell(cellPat, cellAlternatives.toArray)
      // Return
      return Some(out)
    } else {
      return None
    }

  }
  */


  // This is the new, lexicon-based, omni representation (words/lemmas/POS tags), version.
  // Outputs superposition data.
  private def matchConstraintsOmni(row:TableRow, colIdx:Int, cellPat:CellPattern, lexicon:Lexicon[String], varList:VariableList, onlyContentTags:Boolean = true, allowRelaxedConstraints:Boolean = false): Option[Array[FlexiblePatternMatchCell]] = {
    val debugOutput:Boolean = false
    if (debugOutput) {
      println (" ------------------------------------------------------------------------------------ ")
      println ("\n* matchConstraintsOmni: started...")
      println ("Row: " + row.toStringSentWithUID())
      println ("Cell Pattern: " + cellPat.toString())
      println ("")
    }

    // Step 0: Fetch cell text
    // val cellText = row.getCellText(colIdx)

    // Iterate over all possible OR combinations of the cell patterns
    val out = new ArrayBuffer[FlexiblePatternMatchCell]
    for (optionIdx <- 0 until cellPat.getNumOptions()) {
      // Find possible matches of a given cell pattern in the specified cell.  Returns a FlexiblePatternMatchCell, which specifies a superposition of possible matches.
      val possibleMatches = new FlexiblePatternMatchCell(elements = cellPat.getElems(optionIdx), row = row, colIdx = colIdx, OROptionIdx = optionIdx, varList = varList)
      if (debugOutput) {
        println(possibleMatches.toString(lexicon))
      }

      if (possibleMatches.hasMatches) out.append(possibleMatches)
    }

    // Return
    if (out.length > 0) {
      return Some(out.toArray)
    } else {
      return None
    }


  }



/*
  // Private method: Assumes that colIdx is a known-good index.
  private def meetsConstraintHelper(row:TableRow, colIdx:Int, colVal:ColumnValue):Boolean = {
    // Check if column has the same value on at least one of the possibleValues (OR)
    for (possibleValue <- colVal.possibleValues) {
      //println ("Comparing (" + row.getCellText(colIdx) + ") with (" + possibleValue + ")")
      if (row.getCellText(colIdx) == possibleValue) {
        // Match
        //println (" * MATCH")
        return true
      }
    }
    //println (" * Does not meet constraints")
    // Default return
    false
  }
*/


  /*
   * Retrieving all rows from a given table that meet a criterion
   */

  def getAllRowsMatchingConstraint(table:Table, constraints:Array[CellPattern], staticUUIDs:Array[String] = Array.empty[String], varList:VariableList, onlyContentTags:Boolean = true):Array[PatternMatchRowFlex] = {
    val out = new ArrayBuffer[PatternMatchRowFlex]
    val lexicon = table.lexicon
    val debugOutput:Boolean = false

    // Step 1: Find the column indices of columns named in the constraints
    val colIdxs = new Array[Int](constraints.size)
    for (i <- 0 until constraints.size) {
      // Step 1: Double-check if column exists, and get the column index
      val colIdx = table.getColumnIdx( constraints(i).columnName )

      // Return empty array if column is not found
      if (colIdx.isEmpty) {
        // Failure
        logger.error("ERROR: Constraint (column name = " + constraints(i).columnName + ") not found.  Not possible to match this list of constriants. Returning.")
        logger.error("Table: " + table.name)
        logger.error( constraints(i).toString() )
        logger.error("All constraints:")
        for (constraint <- constraints) {
          logger.error(constraint.toString())
        }
        sys.exit(1)
        return Array.empty[PatternMatchRowFlex]
      } else {
        // Success
        colIdxs(i) = colIdx.get
      }

    }


    // Step 2A: Assemble a list of rows to check.  This will either be (a) all rows in the table, or (b) a small list of static rows manually provided.
    var rows = table.rows
    if (!staticUUIDs.isEmpty) {
      rows = new ArrayBuffer[TableRow]()
      for (staticUUID <- staticUUIDs) {
        val row = table.tablestore.getRowByUID(staticUUID)
        if (row.isEmpty) {
          throw new RuntimeException("ERROR: Static UUID referenced ('" + staticUUID + "') could not be found. ")
        }
        rows.append( row )
      }
    }


    // Step 2B: Search for all rows that meet the constriant
    //for (row <- table.rows) {
    for (row <- rows) {
      //## println ("\n\nRow: " + row)
      val matchedConstraints = new ArrayBuffer[Array[FlexiblePatternMatchCell]]

      if (debugOutput) {
        println ("")
        println (" -------------------------------------------------------------------------- ")
        println ("")
        println ("ROW: " + row.toStringSentWithUID())
        println ("")
      }

      breakable {
        for (i <- 0 until constraints.size) {
          val constraint = constraints(i)
          val colIdx = colIdxs(i)

          if (debugOutput) {
            println ("")
            print("* Constraint (" + i + "):  ")
            println(constraint.toString())
          }

          // If this row doesn't meet one of the constraints, do not consider any of the other constraints -- break.
          //val out = matchConstraints(row, colIdx, constraint, lexicon, onlyContentTags)
          val outMCO = matchConstraintsOmni(row, colIdx, constraint, lexicon, varList, onlyContentTags)
          if (outMCO == None) {
            if (debugOutput) println ("Failure to match constraint " + i)
            break
          }

          // TODO: Do something with the constraint matches here -- e.g. store them for later use.
          matchedConstraints.append(outMCO.get)
          if (debugOutput) {
            println("MATCH (Constraint " + i + "):")
            for (matchedConstraint <- outMCO.get) {
              println(matchedConstraint.toString(lexicon))
            }
          }

        }

        // If we reach this point, then the row meets all the constraints. Add it to the list of output rows.
        val pmRow = new PatternMatchRowFlex(row, matchedConstraints.toArray, generatedFromStaticUUID = !staticUUIDs.isEmpty)
        out.append(pmRow)
      }
    }
    if (debugOutput) println ("")

    // Return
    out.toArray
  }


}



// Storage class for potentially flexible span matches (the "superposition" solver)
class FlexibleSpanMatch(val origElem:PatternElem, var start:Int, var until:Int, val colIdx:Int, val altIdx:Int, var areSpansUnsure:Boolean,
                        val varIdx:Int = -1, val spanWords:Array[Int] = Array.empty[Int], val spanLemmas:Array[Int] = Array.empty[Int], val value:OmniValue = new OmniValue(Array.empty[Array[Int]])) {

  var isDone:Boolean = false

  val isRelaxable:Boolean = origElem.isRelaxable
  val isOptional:Boolean = origElem.isOptional

  val isVariable:Boolean = origElem.isVariable
  val isLexicalPattern:Boolean = origElem.isLexicalPattern

  // Check if this span is optional AND has been marked as being skipped/unpopulated (i.e. start span of -1)
  val isOptionalAndSkipped:Boolean = if ((start == -1) && (isOptional)) true else false
  // TODO: Also add accessors for lexical contents?

  /*
   * String methods
   */
  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append("[start: " + start + "\t until: " + until + "\t altIdx: " + altIdx + "\t areSpansUnsure: " + areSpansUnsure + "\t" + origElem.toString())
    if (isVariable) {
      os.append("\t varIdx: " + varIdx + "\t spanWords:(" + spanWords.mkString(", ") + ") " + LexiconUtil.lexiconIdxsToStr(spanWords, lexicon) + "\t spanLemmas:(" + spanLemmas.mkString(", ") + ") " + LexiconUtil.lexiconIdxsToStr(spanLemmas, lexicon))
    }
    os.append("]")

    // Return
    os.toString
  }

}


object FlexibleSpanMatch {

  def mkFlexibleSpan(row:TableRow, origElem:PatternElem, colIdx:Int, altIdx:Int, varList:VariableList, start:Int, until:Int, areSpansUnsure:Boolean, onlyContentTags:Boolean):FlexibleSpanMatch = {
    // Setup precomputed variable spans
    //if (origElem.isVariable) {
      val varIdx = varList.getVariableIdx(origElem.getVariableName)
      val spanWords = row.getCellWordsAlt(colIdx, altIdx, onlyContentTags).slice(start, until)
      val spanLemmas = row.getCellLemmasAlt(colIdx, altIdx, onlyContentTags).slice(start, until)
      val spanCombined = combineWordLemmaArrays(spanWords, spanLemmas)
      return new FlexibleSpanMatch(origElem, start, until, colIdx, altIdx, areSpansUnsure, varIdx, spanWords, spanLemmas, new OmniValue(spanCombined))
    /*
    } else {
      // Return
      return new FlexibleSpanMatch(origElem, start, until, colIdx, altIdx, areSpansUnsure)
    }
    */

  }

  def generateFromSpanArray(in:Array[(Int, Int)], row:TableRow, origElem:PatternElem, colIdx:Int, altIdx:Int, varList:VariableList, areSpansUnsure:Boolean, onlyContentTags:Boolean):Array[FlexibleSpanMatch] = {
    val out = new Array[FlexibleSpanMatch](in.length)
    for (i <- 0 until in.length) {
      out(i) = mkFlexibleSpan(row, origElem, colIdx, altIdx, varList, in(i)._1, in(i)._2, areSpansUnsure, onlyContentTags)
    }
    // Return
    out.toArray
  }

  // Create a single array, where each element is a set of word and lemma lexicon indices for that word position.
  // Each index will have either one or two values -- one if the word and lemma lexicon index are identical, two if those lexicon indices are different.
  // Used to precompute this array for fast matching
  private def combineWordLemmaArrays(words:Array[Int], lemmas:Array[Int]):Array[Array[Int]] = {
    val out = new Array[Array[Int]](words.length)

    for (i <- 0 until words.length) {
      if (words(i) == lemmas(i)) {
        // Same element in both arrays -- store only once
        out(i) = Array[Int](words(i))
      } else {
        // Different element in both arrays -- store both
        out(i) = Array[Int](words(i), lemmas(i))
      }
    }

    // Return
    out
  }

}


class FlexibleSpanMatchCollection(val matches:Array[FlexibleSpanMatch], val colIdx:Int, val altIdx:Int) {
  var relaxedCount:Int = 0


  override def clone():FlexibleSpanMatchCollection = {
    val out = new FlexibleSpanMatchCollection(matches, colIdx, altIdx)
    out.relaxedCount = this.relaxedCount
    // Return
    out
  }

  /*
   * String methods
   */
  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder
    for (i <- 0 until matches.length) {
      os.append("\t" + i + ": " + matches(i).toString(lexicon) + "\n")
    }
    os.append("relaxedCount: " + relaxedCount)
    // Return
    os.toString
  }

}

class FlexiblePatternMatchCell(elements:Array[PatternElem], row:TableRow, colIdx:Int, OROptionIdx:Int, varList:VariableList) {       //## lexicon:Lexicon[String]) {
  val numElements = elements.length
  val numAlternatives = row.getCellNumAlternatives(colIdx)
  val numVariables = countNumVariables()
  val potentialMatchesFlat = initialize()
  val numPotentialMatches = potentialMatchesFlat.length
  val hasVariables = hasVariablesInPattern()

  // Initialize

  /*
   * Accessors
   */
  // TODO: For potential match Y, Get variable X
  // TODO: For potential match Y, get all variables
  // TODO: Compare variables (on words/lemmas) ?  (or place this in the matching code?)
  // TODO: Some kind of indication of how complex this pattern is, that the external matching code can use to determine the order of evaluation
  // TODO: NOTE, matching code will have to take both 'optional' and 'relaxable' variables into account.
  // TODO: Evaluation order: strict variable matching, then relaxable?
  // TODO*: How are relaxable lexical patterns adjacent to variable patterns handled?  Are they currently not handled?  Should this allow the lexical pattern to adaptively "grow" so the variable pattern has less length?
  // TODO*: Check that the potential matches have complete spans (i.e. span the entire cell, and each element chains from the end of the last to the start of the next)? Take into account relaxation in this

  // Returns the number of matches
  def size:Int = numPotentialMatches

  // Returns true if at least one possible match exists
  def hasMatches:Boolean = if (numPotentialMatches <= 0) return false else true

  // Gets a possible match at a particular index (from 0 until size)
  def get(idx:Int):FlexibleSpanMatchCollection = {
    potentialMatchesFlat(idx)
  }


  /*
   * Initialization/Population
   */

  private def hasVariablesInPattern():Boolean = {
    for (elem <- elements) {
      if (elem.isVariable) return true
    }
    // Default return
    false
  }

  private def countNumVariables():Int = {
    var count:Int = 0
    for (i <- 0 until elements.length) {
      if (elements(i).isVariable) count += 1
    }
    // Return
    count
  }

  // Finds all potential matches given this cell pattern
  def initialize(onlyContentTags:Boolean = true):Array[FlexibleSpanMatchCollection] = {
    //## println(" * Initialize(): Started... ")
    val potentialMatches = new Array[Array[Array[FlexibleSpanMatch]]](numAlternatives)   // (alternativeIdx, elementIdx, potentialMatches)

    // Step 1: Lexical matches

    // For each alternative
    val invalidAlts = mutable.Set[Int]()

    for (altIdx <- 0 until numAlternatives) {
      potentialMatches(altIdx) = new Array[Array[FlexibleSpanMatch]](numElements)

      breakable {
        // For each element
        for (elemIdx <- 0 until numElements) {
          val curElement = elements(elemIdx)
          if (curElement.isLexicalPattern) {
            // Find all possible locations where this could match
            val lexPattern = curElement.getLexicalPattern
            var lexMatchSpans =
              if (curElement.isRelaxable) {
                row.cellMatchStrSpanOmniRelaxableIterative(colIdx, altIdx, startFrom = 0, lexPattern, onlyContentTags)
              } else {
                row.cellMatchStrSpanOmniIterative(colIdx, altIdx, startFrom = 0, lexPattern, onlyContentTags)
              }
            // If the element is optional, add a potential "no match" span element
            if (curElement.isOptional) {
              lexMatchSpans ++= Array((-1, -1))
            }

            // If we found no potential matches for this element, and it's non-optional, then return break to the next alternative
            if (lexMatchSpans.isEmpty) {
              invalidAlts.add(altIdx)
              break()
              //return Array.empty[FlexibleSpanMatchCollection]
            }

            potentialMatches(altIdx)(elemIdx) = FlexibleSpanMatch.generateFromSpanArray(lexMatchSpans, row, curElement, colIdx, altIdx, varList, areSpansUnsure = false, onlyContentTags)

          /*
          // Debug printing
          for (i <- 0 until potentialMatches(altIdx)(elemIdx).length) {
            println("\t* " + i + ":\t" + potentialMatches(altIdx)(elemIdx)(i).toString(row.lexicon))
          }
          */


          } else {
            // Variable pattern -- fill in later
            potentialMatches(altIdx)(elemIdx) = Array.empty[FlexibleSpanMatch]
          }
        }
      }

    }

    // Step 2: Variable matches
    val out = new ArrayBuffer[FlexibleSpanMatchCollection]()

    // For each alternative
    for (altIdx <- 0 until numAlternatives) {
      if (!invalidAlts.contains(altIdx)) {
        // Create an iterator to iterate through all combinations of lexical elements
        val maxValues = new Array[Int](numElements)
        for (elemIdx <- 0 until numElements) maxValues(elemIdx) = potentialMatches(altIdx)(elemIdx).length
        val iter = new CombinationIteratorZeroAware(maxValues)
        //## println ("iter.size: " + iter.size)

        val potentialCombinations = new ArrayBuffer[FlexibleSpanMatchCollection]() // List of complete potential combinations, including all variable and lexical assignments for this cell alternative.
        while (iter.hasNext()) {
          // Assemble a list of elements, filled in with only lexical elements from a given potential combination
          val indices = iter.next()
          //## println ("\t indices: " + indices.mkString(", "))
          val elems = new Array[FlexibleSpanMatch](numElements)
          for (elemIdx <- 0 until numElements) {
            if (indices(elemIdx) != -1) elems(elemIdx) = potentialMatches(altIdx)(elemIdx)(indices(elemIdx))
            //## println ("elems(" + elemIdx + "): " + potentialMatches(altIdx)(elemIdx)(indices(elemIdx)).toString(row.lexicon) ) //##
          }



          // Attempt to fill in variable spans
          // mkVariableSpans generates a list of potential valid (complete) combinatoins of lexical and variable spans that could complete this cell pattern.
          // Here we generate them for each possible lexical pattern, and store them.
          if (numVariables > 0) {
            // Pattern contains at least one variable
            potentialCombinations.insertAll(potentialCombinations.length, mkVariableSpans(elems, altIdx, onlyContentTags))
          } else {
            // Special case: Only lexical pattern, contains no variables
            potentialCombinations.append(new FlexibleSpanMatchCollection(elems, colIdx, altIdx))
          }
        }
        // Filter out any potential matches that aren't complete spans, keep track of the relaxation count, and store.
        out.insertAll(out.length, filterCombinationsForCompleteSpans(potentialCombinations, colIdx, altIdx, onlyContentTags))
      }
    }


    // Special case: All elements are optional
    var allElementsOptional:Boolean = true
    breakable {
      for (elemIdx <- 0 until elements.length) {
        if (!elements(elemIdx).isOptional) {
          allElementsOptional = false
          break()
        }
      }
    }
    // If all elements are optional, then the case where there are no matches for any elements has likely been filtered out.  Add in one.
    if (allElementsOptional) {
      val elemOut = new ArrayBuffer[FlexibleSpanMatch]()
      for (elemIdx <- 0 until elements.length) {
        // All elements are optional and mark that no match was found for them
        elemOut.append( new FlexibleSpanMatch(elements(elemIdx), start = -1, until = -1, colIdx, altIdx = 0, areSpansUnsure = false) )
      }
      out.append( new FlexibleSpanMatchCollection(elemOut.toArray, colIdx, altIdx = 0) )
    }

    // Convert to flat array
    //## println(" * Initialize(): Completed... ")

    // Return
    out.toArray
  }


  // Check that each element of the input defines a complete span -- i.e., each word in the cell is covered by at least one element.
  // While doing this, it also calculates the relaxation count of each FlexibleSpanMatchCollection
  private def filterCombinationsForCompleteSpans(in:Traversable[FlexibleSpanMatchCollection], colIdx:Int, altIdx:Int, onlyContentTags:Boolean):ArrayBuffer[FlexibleSpanMatchCollection] = {
    val out = new ArrayBuffer[FlexibleSpanMatchCollection]
    val wordSlots = row.getWordSlotsOmni(colIdx, altIdx, onlyContentTags)
    val numWords = wordSlots.length

    for (potentialMatch <- in) {
      var curIdx = 0
      var relaxationCount = 0
      breakable {
        for (elementIdx <- 0 until potentialMatch.matches.length) {
          val element = potentialMatch.matches(elementIdx)
          if (element.isOptional && element.start == -1) {
            // Element is optional, and not populated in this potential match -- skip

          } else {
            if (element.start != curIdx) {
              // Case 1: Element does not start where last element ended

              // TODO: Incorporate check for relaxable elements, which may not have completely overlapping spans?
              // Check whether this element, or neighbouring elements, are relaxable (and this not required to have strict span connection)
              if ((element.isRelaxable) ||
                ((elementIdx - 1 >= 0) && (potentialMatch.matches(elementIdx - 1).isRelaxable)) ||
                ((elementIdx + 1 < potentialMatch.matches.length) && (potentialMatch.matches(elementIdx + 1).isRelaxable))) {
                // Either this element, or the neighbouring element, is relaxable.  Continue searching the span.
                curIdx = element.until
                relaxationCount += 1

              } else {
                // Not relaxable -- there is a hole in the span, so this pattern is not good.
                break()
              }

            } else {
              // Case 2: Element starts where last element left off -- set current index to the end of this element, and continue
              curIdx = element.until
            }
          }
        }
        // If we reach here, we are at the end of the elements.  Check that we reached the last word.
        if (curIdx != numWords) {
          // The span of the last element does not include the last word.
          // TODO: Incorporate check for relaxable elements, which may not have completely overlapping spans?
          if (potentialMatch.matches.last.isRelaxable) {
            // Last element is relaxable -- it's okay that we didn't reach the end of the list of words.
            relaxationCount += 1
          } else {
            // Last element is not relaxable -- there is a hole in the span.
            break()
          }
        }

        // Set the relaxation count of this pattern
        potentialMatch.relaxedCount = relaxationCount
        //## println ("Relaxation count: " + relaxationCount)

        // The span of the last element includes the last word -- the span is complete.  Store.
        out.append(potentialMatch)

        //## println (out.last.toString(row.lexicon))
      }
    }

    //## println ("filterCombinationsForCompleteSpans: start: " + in.size + "  out after filtering: " + out.length)

    // return
    out
  }

  private def findNextLexicalElement(elementStartIdx:Int):Int = {
    for (i <- elementStartIdx until numElements) {
      if (elements(i).isLexicalPattern) return i
    }
    // Default Return
    -1
  }


  private def mkVariableSpans(elems:Array[FlexibleSpanMatch], altIdx:Int, onlyContentTags:Boolean):ArrayBuffer[FlexibleSpanMatchCollection] = {
    val debugOutput:Boolean = false
    // Step 1: Find length of neighbouring variables spans
    val variableSpans = new ArrayBuffer[(Int, Int)]()     // (start, until)
    var startIdx:Int = -1
    var curIdx:Int = 0
    while (curIdx < numElements) {
      // Start conditions
      if (startIdx == -1) {
        if (elements(curIdx).isVariable) startIdx = curIdx
      }

      // End conditions
      if (startIdx >= 0) {
        if (elements(curIdx).isLexicalPattern && !elems(curIdx).isOptionalAndSkipped) {
          variableSpans.append( (startIdx, curIdx) )
          startIdx = -1
        }
      }

      // Increment
      curIdx += 1
    }
    // Edge case: Cell pattern ends with an open variable span
    if (startIdx >= 0) variableSpans.append( (startIdx, curIdx) )

    val validIterations = new ArrayBuffer[FlexibleSpanMatchCollection]
    val partialIterations = new ArrayBuffer[Array[FlexibleSpanMatch]]
    partialIterations.append(elems)

    if (debugOutput) println ("Variable Spans:")
    for (i <- 0 until variableSpans.length) {
      if (debugOutput) println ("\t" + i + ": " + variableSpans(i))

      val possibleCombiations = mkVariableSpansHelper(elems, variableSpans(i)._1, variableSpans(i)._2, altIdx, onlyContentTags)
      if (debugOutput) println ("\t\tPossible Combinations: " + possibleCombiations.length)
      if (possibleCombiations.length > 0) {
        for (combIdx <- 0 until possibleCombiations.length) {
          for (j <- 0 until partialIterations.length) {
            val combined = new Array[FlexibleSpanMatch](numElements)
            for (k <- 0 until numElements) {
              if (partialIterations(j)(k) != null) combined(k) = partialIterations(j)(k)
            }
            for (k <- 0 until numElements) {
              if (possibleCombiations(combIdx)(k) != null) combined(k) = possibleCombiations(combIdx)(k)
            }

            // Check for completeness and store in relevant array (partially complete or fully complete)
            breakable {
              for (k <- 0 until numElements) {
                if (combined(k) == null) {
                  // Pattern isn't yet complete, store in partial iterations
                  partialIterations.append(combined)
                  break()
                }
              }
              // If we reach here, pattern is complete -- store complete
              validIterations.append( new FlexibleSpanMatchCollection(combined, colIdx, altIdx) )
            }
          }
        }
      }
    }

    /*
    // Completeness
    println ("Number of valid iterations: " + validIterations.length)
    for (i <- 0 until validIterations.length) {
      println ("\tValid Iteration " + i)
      for (j <- 0 until validIterations(i).length) {
        print("\t\t" + j + ": ")
        if (validIterations(i)(j) != null) {
          println(validIterations(i)(j).toString())
        } else {
          println ("null")
        }
      }
    }
     */

    // Return
    validIterations
  }


  private def mkVariableSpansHelper(elems:Array[FlexibleSpanMatch], elementStartIdx:Int, elementUntilIdx:Int, altIdx:Int, onlyContentTags:Boolean):Array[Array[FlexibleSpanMatch]] = {
    // Step 1: Determine the start and end spans (in terms of number of words)
    val wordSlots = row.getWordSlotsOmni(colIdx, altIdx, onlyContentTags)

    // Step 1A: Determine the start index (referenced in terms of words)
    var startWordIdx = -1
    var startElementIdx = elementStartIdx
    breakable {
      while (startElementIdx >= 0) {
        if (elems(startElementIdx) != null) {
          if (elems(startElementIdx).until >= 0) {
            startWordIdx = elems(startElementIdx).until
            break()
          }
        }
        startElementIdx -= 1
      }
      // If we reach here, we reached the start of elements.  The beginning span starts at the first word in the cell.
      startWordIdx = 0
    }

    // Step 1B: Determine the end index (referenced in terms of words)
    var endWordIdx = -1
    var endElementIdx = elementUntilIdx
    breakable {
      while (endElementIdx < numElements) {
        if (elems(endElementIdx) != null) {
          if (elems(endElementIdx).start >= 0) {
            endWordIdx = elems(endElementIdx).start
            break()
          }
        }
      }
      // If we reach here, we reached the end of elements.  The end span ends at the last word in the cell.
      endWordIdx = wordSlots.length
    }

    //## println ("startWordIdx: " + startWordIdx + "  endWordIdx: " + endWordIdx)

    // Step 2: If there is more than one variable element adjacent in this span, then enumerate all possible lengths for each of the variables.

    // Step 2A: Count the number of variables
    var numVariables:Int = 0
    var numOptionalVariables:Int = 0
    for (elemIdx <- elementStartIdx until elementUntilIdx) {
      if (elements(elemIdx).isVariable) {
        numVariables += 1
        if (elements(elemIdx).isOptional) {
          numOptionalVariables += 1
        }
      }
    }

    // Step 2B: Determine the length of the span (in words)
    val spanLength = endWordIdx - startWordIdx

    // Step 2C: Test condition -- if there are too few words to have at least one word in all (non-optional) variables, then exit without computing enumerations to save time
    if ((numVariables - numOptionalVariables) > spanLength) {
      //## println ("break on too few words")
      return Array.empty[Array[FlexibleSpanMatch]]
    }

    // Create an iterator for each variable in this span
    val maxLength = Array.fill[Int](numElements)(0)
    for (elemIdx <- elementStartIdx until elementUntilIdx) {
      if (elements(elemIdx).isVariable) {
        maxLength(elemIdx) = spanLength+1
        //## if (elements(elemIdx).isOptional) maxLength(elemIdx) += 1     // If the variable is optional, add 1 to the span length -- this extra value will signify the variable is not populated
      }
    }


    val validIterations = new ArrayBuffer[Array[FlexibleSpanMatch]]

    val iter = new CombinationIteratorZeroAware(maxLength)
    while (iter.hasNext()) {
      val varLengths = iter.next()
      //## println ("varLengths: " + varLengths.mkString(", "))

      breakable {

        // Calculate total length of this combination
        var totalLength = 0
        for (elemIdx <- elementStartIdx until elementUntilIdx) {
          if (elements(elemIdx).isVariable) {
            val varLength = varLengths(elemIdx)
            // If a variable element's length is zero, and it's not optional, then this is not a valid combination -- break
            if ((varLength == 0) && !elements(elemIdx).isOptional) break()
            totalLength += varLength
          }
        }
        // If the total length is not equal to the length of the span, then this is not a valid combiation -- break
        if (totalLength != spanLength) break()

        // If we reach here, then the combination has the correct length.  Record it.
        //## println ("correct length (" + spanLength + "): recording")
        var curIdx = startWordIdx
        val out = new Array[FlexibleSpanMatch](numElements)
        for (elemIdx <- elementStartIdx until elementUntilIdx) {
          if (elements(elemIdx).isVariable) {
            val varLength = varLengths(elemIdx)
            out(elemIdx) = FlexibleSpanMatch.mkFlexibleSpan(row, elements(elemIdx), colIdx, altIdx, varList, start = curIdx, until = curIdx + varLength, areSpansUnsure = false, onlyContentTags)     //## TODO: AreSpansUnsure == false -- is this true?
            curIdx += varLength
          }
        }
        validIterations.append(out)
      }

    }

    // Edge case: all are optional
    if ((numVariables > 0) && ((numVariables - numOptionalVariables) == 0)) {
      val out = new Array[FlexibleSpanMatch](numElements)
      for (elemIdx <- elementStartIdx until elementUntilIdx) {
        if (elements(elemIdx).isVariable) {
          out(elemIdx) = FlexibleSpanMatch.mkFlexibleSpan(row, elements(elemIdx), colIdx, altIdx, varList, start = -1, until = -1, areSpansUnsure = false, onlyContentTags)     //## TODO: AreSpansUnsure == false -- is this true?
        }
      }
      validIterations.append(out)
    }

    // Return
    validIterations.toArray
  }


  /*
   * String methods
   */

  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    // Debug display
    os.append("Num Potential Matches: " + numPotentialMatches + "\n")
    if (numPotentialMatches > 0) {
      for (i <- 0 until numPotentialMatches) {
        os.append("\tValid Iteration " + i + "\n")
        os.append(potentialMatchesFlat(i).toString(lexicon) + "\n")
      }
    } else {
      os.append("No valid matches found. \n")
    }

    // Return
    os.toString()
  }


}


// Storage class for an "omni-representation", where each word could have several different forms (e.g. word, lemma, POS tag, etc), that are all valid combinations during comparison.
// For example, an omni-value might be ("the cats/cat are/be happy").
// Comparing against another omni-value ("the cat is/be happy") would return true.
// Comparing against another omni-value ("the dog is/be happy") would return false.
class OmniValue(val value:Array[Array[Int]]) {
  val length = value.length

  // Compare two OmniValues
  def compare(in:OmniValue):Boolean = {
    // Check lengths (fast) -- if they're different, the match is not possible
    if (this.length != in.length) return false

    // Check each element
    for (elemIdx <- 0 until length) {
      breakable {
        for (thisElemVal <- this.value(elemIdx)) {
          for (inElemVal <- in.value(elemIdx)) {
            if (thisElemVal == inElemVal) break()     // At least one set of values matches -- the condition for this element is satisfied, so break and move onto the next element
          }
        }
        // If we reach here, no elements at this index matched -- return false
        return false
      }
    }

    // Return
    true
  }

  // Compare two relaxable omnivalues.  Returns their intersection.
  def compareRelaxable(in:OmniValue):(Boolean, Array[Array[Int]]) = {
    // Check lengths (fast) -- if they're different, the match is not possible
    if (this.length != in.length) return (false, Array.empty[Array[Int]])

    // Check each element
    val intersection = new ArrayBuffer[Array[Int]]
    for (elemIdx <- 0 until length) {
      breakable {
        val out = new ArrayBuffer[Int]()
        for (thisElemVal <- this.value(elemIdx)) {
          for (inElemVal <- in.value(elemIdx)) {
            if (thisElemVal == inElemVal) {
              out.append(thisElemVal)
            }
          }
        }
        if (out.length > 0) intersection.append(out.toArray)
      }
    }

    if (intersection.length > 0) {
      return (true, intersection.toArray)
    } else {
      return (false, Array.empty[Array[Int]])
    }
  }

  /*
   * Cloning
   */
  // TODO: Untested -- ensure this makes deep copies
  override def clone():OmniValue = {
    return new OmniValue(value.clone())
  }

  /*
   * String methods
   */
  def toStringValueOnly(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (i <- 0 until value.length) {
      val valueToUse = value(i).last      // Take the last, which should likely be the lemma
      os.append(lexicon.get(valueToUse))
      if (i < value.length-1) os.append(" ")
    }

    os.toString()
  }

  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    //os.append("[L=" + value.length + "]")

    os.append("(")
    for (i <- 0 until value.length) {
      os.append(value(i).mkString("/"))
      if (i < value.length-1) os.append(" ")
    }
    os.append(") ")

    for (i <- 0 until value.length) {
      for (j <- 0 until value(i).length) {
        os.append(lexicon.get(value(i)(j)))
        if (j < value(i).length-1) os.append("/")
      }
      if (i < value.length-1) os.append(" ")
    }

    // Return
    os.toString
  }

}


object OmniValue {

  def mkFrom1DArray(in:Array[Int]) = {
    val repacked = new Array[Array[Int]](in.length)
    for (i <- 0 until in.length) {
      repacked(i) = Array(in(i))
    }
    // Return
    new OmniValue(repacked)
  }


  def main(args:Array[String]): Unit = {
    val lexicon = new Lexicon[String]
    val sentence = "the cat ran after the dog"

    val sentLex = LexiconUtil.strToLexiconIdxs(sentence, lexicon)

    val omni = mkFrom1DArray(sentLex)

    println ("Sentence: " + sentence)

    println ("OmniValue.toString(): ")
    println (omni.toString(lexicon))

  }

}
