package inferenceengine.iml.constraint

import inferenceengine.struct._
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.extension.Tuples
import InfPatConstraintFinder.{ROWVAR_PREFIX, WILDCARD_INT}
import edu.arizona.sista.struct.Lexicon
import explanationgraph.{CombinationIterator, TableStore}
import inferenceengine.CombinationIteratorZeroAware
import inferenceengine.iml.runtime.TimeLimit
import org.chocosolver.solver.variables.IntVar

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._


class InfPatConstraintFinder(infPat:InferencePattern, tablestore:TableStore, maxMatches:Int = 100000) {
  // Variable-to-ConstraintTable-Column-Idx look-up-table
  val variableLUT = new VariableList()

  // Internal lexicon to map individual integers to arrays of lexicon IDs, for fast lookup of multiword sequences
  val internalLexicon = new Lexicon[List[Int]]
  internalLexicon.add(List(-1))   // Add impossible element to occupy element 0 (the wildcard)

  // Number of rows in pattern
  val numRowsInPattern = infPat.rowPatterns.length

  // Variables in constraint satisfaction model
  var modelVars:Array[IntVar] = null

  // Valid combinations
  //val validValues = new ArrayBuffer[Array[Int]]
  //val validCombos = new ArrayBuffer[Array[Int]]


  /*
   * Constructor
   */

  // Step 1: Find variables in inference pattern, and populate VariableLUT
  populateVariableLUT()

  // Step 2: Make (/keep) a list of specific table rows that are possible first-pass fits for each row in the inference pattern
  val candidates = findCandidateRows()

  // Step 3: Create the tuples.
  //## populateModelExhaustiveSearch()
  println ("")
  println (" ----------------------------------------------------------------------------------------- ")
  println ("")
  println (this.toString())
  println ("")
  println (" ----------------------------------------------------------------------------------------- ")
  println ("")

  //val validMatches = populateModelExhaustiveSearchFlex(tablestore.lexicon)      // Gold standard
  val validMatches = populateModelBacktrackSearchFlex(tablestore.lexicon, maxMatches)         // Fast and likely working okay but not exhaustively tested


  /*
   * Find variables in an inference pattern
   */
  def populateVariableLUT() {
    // Populate variables for row indicies (so we can use the constraint satisfaction framework to solve these too)
    for (i <- 0 until numRowsInPattern) {
      variableLUT.addVariable(ROWVAR_PREFIX + i)
    }

    // Populate actual variables from the inference pattern
    for (i <- 0 until numRowsInPattern) {
      for (varName <- infPat.rowPatterns(i).getVariablesInPattern()) {
        variableLUT.addVariable(varName)
      }
    }
  }


  /*
   * Find candidate rows for inference pattern (inexpensive pass)
   */
  def findCandidateRows():Array[Array[PatternMatchRowFlex]] = {
    // Output: Outer array: for each row, inner array: possible pattern matches

    val outCandidates = new Array[Array[PatternMatchRowFlex]](numRowsInPattern)
    for (i <- 0 until numRowsInPattern) {
      //## outCandidates(i) = infPat.rowPatterns(i).getTableRowsMatchingConstraints(tablestore, infPat.variableList)
      outCandidates(i) = infPat.rowPatterns(i).getTableRowsMatchingConstraints(tablestore, this.variableLUT)

      // Break if the hard time limit has elapsed.
      if (TimeLimit.isTimeElapsed()) {
        println ("User-specified time limit exceeded (" + TimeLimit.timeLimit + " msec).  Exiting")
        sys.exit(1)
      }
    }
    // Return
    outCandidates
  }


  /*
   * Populating tuples
   */

  /*
  // Creates one "smart tuple" for the constraint satisfaction framework, by populating all the elements of the tuple
  // from a given row candidate, while keeping all other aspects as 'wildcards' (stars) in the smart tuple.
  def mkTuplesPatternMatch(in:PatternMatchRow, rowIdx:Int, patternMatchIdx:Int):Array[Int] = {
    // Step 1: Set variables in tuple
    val tuple = Array.fill[Int](variableLUT.size)(WILDCARD_INT)
    for (cellMatch <- in.cellMatches) {
      for (varName <- cellMatch.cellPat.getAllVariables()) {
        val value = cellMatch.getVariableValue(varName, alt = 0)          //TODO: Hardcoded Alt = 0
        if (value.isDefined) {
          val varIdx = variableLUT.getVariableIdx(varName)
          tuple(varIdx) = internalLexicon.add(value.get.toList)
          //tuple(varIdx) = internalLexicon1.add(value.get.mkString(","))

        }
      }
    }

    // Step 2: Keep a reference to the patternMatchIdx that this came from, so we can quickly find it if it's a match
    val rowVarIdx = variableLUT.getVariableIdx(ROWVAR_PREFIX + rowIdx)
    tuple(rowVarIdx) = patternMatchIdx

    // Return
    tuple
  }
   */

  /*
  // New
  def populateModelExhaustiveSearchFlex(lexicon:Lexicon[String]):Array[(Array[Option[PatternMatchRow]], VariableValueLUT)] = {
    val debugOutput:Boolean = true

    if (debugOutput) println ("* populateModelExhaustiveSearchFlex(): started... ")

    // TODO: Check that it's possible for the pattern to be completed (e.g. there are row candidates for each non-optional row slot).  If not, exit early, to save the computation time.

    // Step 1: Create an iterator for the possible combinations of rows
    val numRowsInSlots = new Array[Int](numRowsInPattern)
    for (rowIdx <- 0 until numRowsInPattern) {
      if (infPat.rowPatterns(rowIdx).isOptional) {
        // Row is optional -- add on an extra possibility for "optional row"
        numRowsInSlots(rowIdx) = candidates(rowIdx).length + 1
      } else {
        numRowsInSlots(rowIdx) = candidates(rowIdx).length
      }
    }

    // Create iterator
    val iterRowIdx = new CombinationIterator(numRowsInSlots)

    // Debug: Report size
    if (debugOutput) {
      println("numRowsInSlots: " + numRowsInSlots.mkString(", "))
      println("Iterator size: " + iterRowIdx.size)
    }


    // Step 2: Iterate through possible combinations, checking whether they are valid or not
    // TODO: Handle optional rows
    val out = new ArrayBuffer[(Array[Option[PatternMatchRow]], VariableValueLUT)]

    // TODO: Parallelize this code for speed
    val size = iterRowIdx.size
    var count:Long = 0
    val displayEveryN:Long = 100000
    while (iterRowIdx.hasNext()) {
      val rowIndices = iterRowIdx.next().clone()

      // Convert indices of rows that are optional in this iteration (i.e., that have indices above max length) to -1.
      for (rowIdx <- 0 until numRowsInPattern) {
        if (rowIndices(rowIdx) == candidates(rowIdx).length) rowIndices(rowIdx) = -1
      }

      val (validMatches, farthestRowIdx) = getValidFlexCombinations(rowIndices, lexicon)
      out.insertAll(out.length, validMatches)

      // Progress display
      count += 1
      if (count % displayEveryN == 0) {
        print("  " + (count.toDouble * 100 / size.toDouble).formatted("%3.0f") + "%")
        if (count % (displayEveryN*20) == 0) println ("")
      }
    }

    println ("* populateModelExhaustiveSearchFlex(): completed... found " + out.length + " matches total for " + infPat.name)

    // Return
    out.toArray
  }
   */

  // New
  def populateModelBacktrackSearchFlex(lexicon:Lexicon[String], maxMatches:Int, exhaustiveMode:Boolean = false):Array[(Array[Option[PatternMatchRow]], VariableValueLUT)] = {
    val debugOutput:Boolean = false
    //val debugOutput:Boolean = true

    if (debugOutput) println ("* populateModelBacktrackSearchFlex(): started... ")

    // TODO: Check that it's possible for the pattern to be completed (e.g. there are row candidates for each non-optional row slot).  If not, exit early, to save the computation time.

    // Step 1: Create an iterator for the possible combinations of rows
    val numRowsInSlots = new Array[Int](numRowsInPattern)
    for (rowIdx <- 0 until numRowsInPattern) {
      if (infPat.rowPatterns(rowIdx).isOptional) {
        // Row is optional -- add on an extra possibility for "optional row"
        numRowsInSlots(rowIdx) = candidates(rowIdx).length + 1
      } else {
        numRowsInSlots(rowIdx) = candidates(rowIdx).length
      }
    }

    // Create iterator
    val iterRowIdx = new CombinationIteratorSkippable(numRowsInSlots)

    // Debug: Report size
    if (debugOutput) {
      println("numRowsInSlots: " + numRowsInSlots.mkString(", "))
      println("Iterator size: " + iterRowIdx.size)
    }

    // Step 2: Iterate through possible combinations, checking whether they are valid or not
    // TODO: Handle optional rows
    val out = new ArrayBuffer[(Array[Option[PatternMatchRow]], VariableValueLUT)]

    // Ensure the iterator is valid
    if (iterRowIdx.size == 0) {
      println ("No valid iterations possible. ")
      return out.toArray
    }

    // TODO: Parallelize this code for speed
    val size = iterRowIdx.size
    var count:Long = 0
    val displayEveryN:Long = 100000
    var skipAtIdx:Int = -1      // Backtracking implementation
    breakable {
      while (!iterRowIdx.isAtEnd()) {
        // If set to exhaustive mode for debugging, always reset the skip idx
        if (exhaustiveMode) skipAtIdx = -1

        val rowIndices = if (skipAtIdx == -1) {
          // Normal iteration
          iterRowIdx.next().clone()
        } else {
          // Backtracking iteration
          iterRowIdx.nextSkip(skipAtIdx).clone()
        }
        skipAtIdx = -1 // Reset

        // Convert indices of rows that are optional in this iteration (i.e., that have indices above max length) to -1.
        for (rowIdx <- 0 until numRowsInPattern) {
          if (rowIndices(rowIdx) == candidates(rowIdx).length) rowIndices(rowIdx) = -1
        }

        // Iterate through OR options
        val numFlexOROptions = new ArrayBuffer[Int]
        for (rowIdx <- 0 until numRowsInPattern) {
          if (rowIndices(rowIdx) != -1) {
            val pmrFlex = candidates(rowIdx)(rowIndices(rowIdx))
            for (relColIdx <- 0 until pmrFlex.getNumRelevantColumns()) {
              val numOptions = pmrFlex.getNumPotentialFlexMatches(relColIdx)
              numFlexOROptions.append(numOptions) // Store length to iterate
            }
          }

        }

        if (debugOutput) println(" * numFlexOrOptions: " + numFlexOROptions.mkString(", "))

        val orIter = new CombinationIterator(numFlexOROptions.toArray)
        var farthestRowIdx: Int = Int.MaxValue
        while (orIter.hasNext()) {
          val orIterIdx = orIter.next()

          if (debugOutput) println("--------------------------------------\n\n\n")
          if (debugOutput) println(" * orIterIdx: " + orIterIdx.mkString(", "))

          val (validMatches, farthestRowIdx1) = getValidFlexCombinations(rowIndices, orIterIdx, lexicon)

          if (debugOutput) println(" * numValidMatches: " + validMatches.length)

          out.insertAll(out.length, validMatches)

          // Store the backtracking index, if applicable
          if ((farthestRowIdx1 == -1) || (farthestRowIdx == -1)) {
            farthestRowIdx == -1
          } else {
            if (farthestRowIdx == Int.MaxValue) {
              farthestRowIdx = farthestRowIdx1
            } else {
              farthestRowIdx = math.max(farthestRowIdx, farthestRowIdx1)
            }
          }
        }

        //## TODO: NOTE: CURRENTLY BROKEN.  Must be missing valid enumerations.
        // POSSIBILITIES:
        // (a) iterator is broken, missing possible valid iterations at end of the list.
        // (b) This method of detecting when to skip is bad -- should possibly check for no matches, AND a match that was specifically caused by variables not matching
        // (c) ALSO TODO: if variables in the rows are optional, do not skip?
        //## println (length + "\t" + rowIndices.mkString(", "))
        if ((farthestRowIdx > -1) && (farthestRowIdx != Int.MaxValue)) {
          skipAtIdx = farthestRowIdx
          //## println ("skipping")
        }


        // Progress display
        count += 1
        if (count % displayEveryN == 0) {
          print("  " + (count.toDouble * 100 / size.toDouble).formatted("%3.0f") + "%")
          if (count % (displayEveryN * 20) == 0) println("")
        }

        // Break if the hard time limit has elapsed.
        if (TimeLimit.isTimeElapsed()) {
          println("User-specified time limit exceeded (" + TimeLimit.timeLimit + " msec).  Exiting")
          sys.exit(1)
        }

        if (out.length > maxMatches) {
          val percentStr = (count.toDouble * 100 / size.toDouble).formatted("%3.0f") + "%"
          println(" * populateModelBacktrackSearchFlex(): Number of valid enumerations generated (" + out.length + ") exceeds manually specified limit (" + maxMatches + ").  Stopping enumeration process at " + percentStr)
          break()
        }
      }
    }

    println ("")
    println ("* populateModelBacktrackSearchFlex(): completed... found " + out.length + " matches total for " + infPat.name)



    // Return
    out.toArray
  }


  def getValidFlexCombinations(rowIndices:Array[Int], OROptionFlexIdxs:Array[Int], lexicon:Lexicon[String]):(Array[(Array[Option[PatternMatchRow]], VariableValueLUT)], Int) = {
    val debugOutput:Boolean = false
    //val debugOutput:Boolean = true
    var orOptionIncIdx:Int = 0

    if (debugOutput) println (" * getValidFlexCombinations: Started... (" + rowIndices.mkString(", ") + ")")


    // Step 1: Populate set of rows -- note, potentially remove this for speed later and do checks in-place
    val rowCandidates = new Array[Option[PatternMatchRowFlex]](numRowsInPattern)
    for (rowIdx <- 0 until numRowsInPattern) {
      //if (infPat.rowPatterns(rowIdx).isOptional) && (rowIndices) {
      if (rowIndices(rowIdx) != -1) {
        // Normal case
        rowCandidates(rowIdx) = Some(candidates(rowIdx)(rowIndices(rowIdx)))
      } else {
        // Row is marked optional in this case
        rowCandidates(rowIdx) = None
      }
      //}
    }

    if (debugOutput) {
      println ("Rows: ")
      for (rowIdx <- 0 until numRowsInPattern) {
        if (rowCandidates(rowIdx).isDefined) {
          println(rowIdx + "\t : " + rowCandidates(rowIdx).get.row.toStringText() )
        } else {
          println(rowIdx + "\t : " + "None")
        }
      }
      println ("")
    }

    // Step 2: Create an iterator for all the possible flexible combinations of these rows
    // Note that we have to iterate over (a) each cell in a row, and (b) for each cell, all it's combinations (i.e. a 2D iteration)
    val flexPossibilities = new ArrayBuffer[Int]
    val flexMappings = new ArrayBuffer[(Int, Int, Int)] //   (RowIdx, cellMatchIdx, orOptionIncIdx)
    for (rowIdx <- 0 until numRowsInPattern) {
      if (debugOutput) println ("RowIdx: " + rowIdx)
      if (rowCandidates(rowIdx).isDefined) {
        val candidate = rowCandidates(rowIdx).get
        //val cellMatches = candidate   //## (OROptionFlexIdxs(rowIdx))    //##### 0 should iterate through OR options

        /*
        if (debugOutput) {
          println ("candidate(rowIdx: " + rowIdx + "): " + candidate.toString(lexicon) )
        }
        */

        for (cellMatchIdx <- 0 until candidate.getNumRelevantColumns()) {
          val optionIdx = OROptionFlexIdxs(orOptionIncIdx)
          val cellMatch = candidate.getPotentialFlexMatches(cellMatchIdx)(optionIdx)
          if (debugOutput) {
            println("cellMatch(rowIdx = " + rowIdx + " cellMatchIdx = " + cellMatchIdx + "): " + cellMatch.toString(lexicon))
          }

          // Check to see if this pattern contains at least one variable, to make sure that it's relevant
          //if (cellMatch.hasVariables) {       // TODO: This effectively filters out all lexical patterns, which are important for downstream relaxation measurement/display, so commented this out.
          flexPossibilities.append(cellMatch.size)
          val tuple = (rowIdx, cellMatchIdx, optionIdx) // (rowIdx, cellMatchIdx)
          flexMappings.append(tuple)
          //}
          orOptionIncIdx += 1
        }
      }

    }

    val iter = new CombinationIterator(flexPossibilities.toArray)

    if (debugOutput) {
      println ("flexPossibilities: " + flexPossibilities.mkString(", "))
      println ("iter.size: " + iter.size)
    }


    // Output of the matching process
    val out = new ArrayBuffer[(Array[Option[PatternMatchRow]], VariableValueLUT)]

    // If this set of rows doesn't match, keep track of the which set of rows broke the iteration so we can skip over the rest
    var farthestRow:Int = -1

    // Step 3: Iterate through possible flexible combinations of these rows
    while (iter.hasNext()) {
      val combination = iter.next()
      // Reset OR option index

      // Keep track of which flex posisbilities were checked
      val whichChecked = new Array[ArrayBuffer[FlexibleSpanMatchCollection]](numRowsInPattern)
      for (i <- 0 until whichChecked.length) whichChecked(i) = new ArrayBuffer[FlexibleSpanMatchCollection]

      breakable {
        // Check to see whether the variable combinations of this version are consistent
        val variableValues = Array.fill[Option[OmniValue]](variableLUT.size)(None)

        for (combElemIdx <- 0 until combination.length) {
          if (debugOutput) {
            println (" combElemIdx: " + combElemIdx + " / " + combination.length)
          }
          // Unpack indices
          val rowIdx = flexMappings(combElemIdx)._1
          val cellIdx = flexMappings(combElemIdx)._2
          val orOptionIdx = flexMappings(combElemIdx)._3
          val whichPossibility = combination(combElemIdx)

          // Grab data
          val pmFlex = rowCandidates(rowIdx).get
          //## println ("orOptionIdx: " + orOptionIdx)
          //## println ("pmFlex.getPotentialMatches(cellIdx).length: " + pmFlex.getPotentialFlexMatches(cellIdx).length)
          val cellMatch = pmFlex.getPotentialFlexMatches(cellIdx)(orOptionIdx)      // Handle the OR option addition
          val flexCellMatch = cellMatch.get(whichPossibility)
          whichChecked(rowIdx).append(flexCellMatch)

          // Check elements
          //println ("Checking elements... (size = " + flexCellMatch.matches.length + ")")
          for (elemMatch <- flexCellMatch.matches) {
            if (elemMatch.isVariable) {
              val varIdx = elemMatch.varIdx
              val value = elemMatch.value

              //println ("varIdx: " + varIdx)
              if (varIdx == -1) {
                println ("ERROR: varIdx: " + varIdx)
                println ("ERROR: Variable name not found: " + elemMatch.toString(lexicon))
                /*
                println ("Variable Values: ")
                for (i <- 0 until variableLUT.size) {
                  val varName = variableLUT.getVariableAtIdx(i)
                  if (variableValues(i).isDefined) {
                    val variableValue = variableValues(i).get.toString(lexicon)
                    println("\tVar" + i + " (" + varName + "): " + variableValue)
                  } else {
                    println("\tVar" + i + " (" + varName + "): None")
                  }
                }
                println("")

                //sys.exit(1)
                */
              }

              if (varIdx >= 0) {
                if (variableValues(varIdx).isDefined) {
                  // There is an existing value for this variable -- check to make sure that it matches with the value we've just found in this row/cell
                  if (elemMatch.isOptional && value.length == 0) {
                    // Variable is optional and does not appear to be populated -- skip
                  } else {
                    // Variable is not optional, or is optional but populated.
                    if (!elemMatch.isRelaxable) {
                      // Not relaxable
                      if (!variableValues(varIdx).get.compare(value)) {
                        farthestRow = math.max(farthestRow, rowIdx) //##
                        if (debugOutput) {
                          println("## Variable does not match: " + varIdx + ":" + variableLUT.getVariableAtIdx(varIdx) + " : " + value.toString(lexicon))
                          println("\tExisting value: " + variableValues(varIdx).get.toString(lexicon))
                        }
                        break()
                      }
                    } else {
                      // Relaxable -- store the intersection between the current Omni value and the found omnivalue
                      val (compatible, intersection) = variableValues(varIdx).get.compareRelaxable(value)
                      if (compatible) variableValues(varIdx) = Some(new OmniValue(intersection))
                    }
                  }
                } else {
                  // There is no existing value for this variable -- populate the new value
                  if (elemMatch.isOptional && value.length == 0) {
                    // Variable is optional and does not appear to be populated in pattern -- skip
                  } else {
                    // Variable is not optional, or is optional but populated -- populate the new value in the variable assignment table
                    variableValues(varIdx) = Some(value)
                  }
                }
              } else {
                // Variable index not found (TODO: Double check that this is not a bug)
                if (elemMatch.isOptional && value.length == 0) {
                  // Variable is optional and does not appear to be populated in pattern -- skip
                } else {
                  // Variable is not optional, and variable index not found in LUT -- this is likely an error
                  println ("ERROR: varIdx: " + varIdx)
                  println ("ERROR: Variable name not found: " + elemMatch.toString(lexicon))
                  throw new RuntimeException("ERROR: varIdx: " + varIdx)
                }
              }
            }
          }
        }
        // If we reach here, then the entire pattern was checked and the variable assignments are consistent.  We likely have a match.


        // TODO: Store successful pattern match -- rows, variable LUT, etc.

        // Step 3B: Populate variable value look-up table
        val variableValueLUT = new VariableValueLUT(lexicon)
        // Copy all variables from flat array to variableLUT structure
        for (i <- 0 until variableLUT.size) {
          if (variableValues(i).isDefined) {
            val varName = variableLUT.getVariableAtIdx(i)
            val variableValue = variableValues(i).get
            variableValueLUT.setVariable(varName, variableValue)
          }
        }

        // Step 3C: Populate variable pattern matches
        val pmr = new Array[Option[PatternMatchRow]](numRowsInPattern)
        for (rowIdx <- 0 until rowCandidates.length) {
          if (rowCandidates(rowIdx).isDefined) {
            val candidate = rowCandidates(rowIdx).get
            val cellMatches = whichChecked(rowIdx).toArray
            val generatedFromStaticUUID = infPat.rowPatterns(rowIdx).hasStaticUUIDs
            pmr(rowIdx) = Some(new PatternMatchRow(row = candidate.row, cellMatches, generatedFromStaticUUID))
          } else {
            // Optional row
            pmr(rowIdx) = None
          }
        }

        // Record match
        out.append( (pmr, variableValueLUT) )



        // Debug output
        if (debugOutput) {
          // debug Hack to get a lexicon reference
          var lexicon: Lexicon[String] = null
          breakable {
            for (i <- 0 until rowCandidates.length) {
              if (rowCandidates(i).isDefined) {
                lexicon = rowCandidates(i).get.row.lexicon
                break
              }
            }
          }

          if (lexicon != null) {
            println("Variable consistency check passed -- match")
            for (i <- 0 until numRowsInPattern) {
              print("\t" + i + ": ")
              if (rowCandidates(i).isDefined) {
                println(rowCandidates(i).get.row.toStringSentWithUID(""))
              } else {
                println ("None")
              }
            }
            println("Variable values: ")
            for (i <- 0 until variableLUT.size) {
              val varName = variableLUT.getVariableAtIdx(i)
              if (variableValues(i).isDefined) {
                val variableValue = variableValues(i).get.toString(lexicon)
                println("\tVar" + i + " (" + varName + "): " + variableValue)
              } else {
                println("\tVar" + i + " (" + varName + "): None")
              }
            }
            println("")
          } else {
            println ("Error: Could not get lexicon reference -- this should never happen.")
          }
        }

      }

    }

    // If we have successful matches, then the farthest row reached before a match broke should be reset
    if (out.length > 0) farthestRow = -1

    /*
    if (out.length > 0) {
      println(rowIndices.mkString(","))
    }
     */

    // Return
    (out.toArray, farthestRow)
  }

  /*
  //##
  // An exhaustive search for valid combinations.
  // This is expensive, but uses a lexicon hashmap for variable lookup (that keeps entire strings as a single integer),
  // so it's reasonably quick for an exhaustive search.
  // TODO: This exhaustive search is inefficient, and there are likely much faster ways of doing this.
  def populateModelExhaustiveSearch() {
    val debugOutput:Boolean = true

    println ("* populateModelExhaustiveSearch: started... ")
    if (validCombos.length > 0) validCombos.clear()
    if (validValues.length > 0) validValues.clear()

    // Step 2: Create tuples
    // Note: To speed up constraint checking/comparison, the entire multi-word contents of a variable (e.g. "the orange cat") is reduced to a single lexicon ID (e.g. 5).
    val tuples = new Array[ArrayBuffer[Array[Int]]](numRowsInPattern)
    for (rowIdx <- 0 until numRowsInPattern) {
      tuples(rowIdx) = new ArrayBuffer[Array[Int]]

      for (pmIdx <- 0 until candidates(rowIdx).length) {
        val tuple = mkTuplesPatternMatch( candidates(rowIdx)(pmIdx), rowIdx = rowIdx, patternMatchIdx = pmIdx )
        tuples(rowIdx).append(tuple)

        // Debug statements
        if (debugOutput) {
          print("Tuples(" + rowIdx + ")(" + tuples(rowIdx).size + "): ")
          for (a <- 0 until tuple.size) {
            print(variableLUT.getVariableAtIdx(a) + ":" + tuple(a) + ", ")
          }
          print("\t" + candidates(rowIdx)(pmIdx).toString(lexicon = tablestore.lexicon))
        }
      }
    }

    val sizes = new Array[Int](numRowsInPattern)
    for (i <- 0 until numRowsInPattern) {
      sizes(i) = tuples(i).size
    }

    val iter = new CombinationIteratorZeroAware(sizes)
    //##val debugTraces = new ArrayBuffer[String]
    if (iter.size > 10000000) {
      println("* populateModelExhaustiveSearch: NOTE, pattern has a large number of possibe iterations (" + iter.size + "). Consider adding additional row-level constraints if possible to speed execution. ")
      println("* populateModelExhaustiveSearch: Number of rows in patterns: ")
      for (rowIdx <- 0 until numRowsInPattern) {
        println(rowIdx + "\t" + infPat.rowPatterns(rowIdx).name + "\t" + candidates(rowIdx).length)
      }
    }

    println ("* populateModelExhaustiveSearch: size = " + iter.size)
    var matchesChecked:Long = 0
    if (sizes.indexOf(0) == -1) {     // ensure there are no zero sizes for any row options, making the pattern unsolvable
      while (iter.hasNext()) {
        val combo = iter.next()
        val (isValidMatch, values) = isValid(tuples, combo)
        if (isValidMatch) {
          validValues.append(values)
          validCombos.append(combo.clone())
          //##debugTraces.append(debugTrace)
        }
        matchesChecked += 1
        if (matchesChecked % 1000000 == 0) {
          val percent = (matchesChecked.toDouble / iter.size.toDouble) * 100
          println("* populateModelExhaustiveSearch: Checked " + matchesChecked + " of " + iter.size + " [" + percent.formatted("%3.3f") + "%] (" + validCombos.size + " valid combinations found)")
        }
      }
    }

    /*
    println ("")
    println ("tuples.size: " + tuples.size)
    println ("sizes: " + sizes.mkString(","))
    println ("iter.size: " + iter.size)
    println ("valid.size: " + validValues.size)
    for (i <- 0 until math.min(20, validValues.size)) {
      println ("\tvalidValues(" + i + "): " + validValues(i).mkString(","))
      println ("\t\tvalidCombos(" + i + "): " + validCombos(i).mkString(","))
      //##println ("\t\tdebugTrace(" + i + "): " + debugTraces(i))
      //println ("\t\t" + displayMatch(validCombos(i), validValues(i)))
      println ("")
    }
     */
    println ("* populateModelExhaustiveSearch: Found " + validValues.length + " shortlisted potential combinations for (" + infPat.name + ").")

  }
   */

  /*
  //##
  def isValid(tuples:Array[ArrayBuffer[Array[Int]]], combo:Array[Int]):(Boolean, Array[Int]) = {
    val values = Array.fill[Int](variableLUT.size)(0)
    //##val os = new StringBuilder      // debug trace

    //##os.append("start\n")

    for (i <- 0 until combo.size) {
      //##os.append("i = " + i + "  values: " + values.mkString(",") + "\n")
      val toAdd = tuples(i)(combo(i))
      //##os.append (" toadd: " + toAdd.mkString(",") + "\n")
      for (j <- 0 until variableLUT.size) {
        if (toAdd(j) != 0) {
          if (values(j) == 0) {
            values(j) = toAdd(j)
          } else {
            if (values(j) != toAdd(j)) {
              //##os.append(" doesNotFit: (j = " + j + "). returning false.\n")
              return (false, values)
            }

          }
        }
      }
      //##os.append(" finished. values: " + values.mkString(",") + "\n")
    }

    // If we make it here, the pattern is valid
    return (true, values)
  }

   */

  /*
  // Convert the valid combinations into an array of Arrays of PatternMatchRows, which are more easily used by other
  // classes.
  // TODO: Unsure how the row variable indices are used here
  def getValidCombos():Array[Array[PatternMatchRow]] = {
    val out = new ArrayBuffer[Array[PatternMatchRow]]

    /*
    for (i <- 0 until validValues.length) {
      val rows = new Array[PatternMatchRow](numRowsInPattern)

      for (j <- 0 until numRowsInPattern) {
        val rowVarIdx = variableLUT.getVariableIdx(ROWVAR_PREFIX + j)
        val rowIdx = validValues(i)(rowVarIdx)
        rows(j) = candidates(j)(rowIdx)
      }

      out.append(rows)
    }
    */

    // Return
    out.toArray
  }
  */

  //##
  /*
  def displayMatch(combo:Array[Int], values:Array[Int]):String = {
    val os = new StringBuilder

    // Get rows
    val rows = new Array[PatternMatchRow](numRowsInPattern)
    for (i <- 0 until numRowsInPattern) {
      val rowVarIdx = variableLUT.getVariableIdx(ROWVAR_PREFIX + i)
      val rowIdx = values(rowVarIdx)
      rows(i) = candidates(i)(rowIdx)
    }

    os.append("Variables:\n")
    for (i <- 0 until variableLUT.size) {
      val varName = variableLUT.getVariableAtIdx(i)
      if (!varName.startsWith(ROWVAR_PREFIX)) {
        val valueIntLex = internalLexicon.get(values(i))
        os.append("\t" + varName.formatted("%20s") + ": " + valueIntLex.mkString(", ") + "\n")
      }
    }

    os.append("Rows: \n")
    for (i <- 0 until numRowsInPattern) {
      os.append("Row " + i + " (" + infPat.rowPatterns(i).name + "):" + rows(i).toString() + "\n")
    }


    // return
    os.toString()
  }
  */

  /*
   * String methods
   */
  override def toString():String = {

    //minimalExample()
    //sys.exit(1)


    val os = new StringBuilder
    os.append("Inference Pattern: " + infPat.name + "\n")

    os.append("Variables: \n")
    for (i <- 0 until variableLUT.size) {
      os.append("\t" + variableLUT.getVariableAtIdx(i) + "\n")
    }

    os.append("Candidate Row Sizes: \n")
    for (i <- 0 until numRowsInPattern) {
      os.append("\tRow Slot " + i + ": " + candidates(i).length + " rows \n")
    }

    os.append("Candidate Row Examples: \n")
    for (i <- 0 until numRowsInPattern) {
      os.append("\tRow Slot " + i + ": " + candidates(i).length + " rows \n")
      for (j <- 0 until math.min(5, candidates(i).length)) {
        os.append("\t\t" + j + ": " + candidates(i)(j).row.toStringSentWithUID() + "\n")
      }
    }


    /*
    os.append("Candidate Row Examples: \n")
    for (i <- 0 until numRowsInPattern) {
      os.append("\tRow " + i + ": " + candidates(i).length + " rows \n")

      for (j <- 0 until math.min(candidates(i).length, 10)) {
        os.append("\t\tCandidate (" + j + "): " + candidates(i)(j).toString + "\n")

        os.append(" Smart Tuple: " + mkTuplesPatternMatch(candidates(i)(j), rowIdx = i, patternMatchIdx = j).mkString(",") + "\n" )
      }

    }

     */

    //populateModelExhaustiveSearch()   //##
    //val model = populateModel()
    //solveModel(model)


    // Return
    os.toString()
  }

}


object InfPatConstraintFinder {
  // Row Prefix in variable LUT
  val ROWVAR_PREFIX     =   "ROWPAT_IDX"

  // "Star" (wildcard) tuple for smart tuples in constraint satisfaction framework
  val WILDCARD_INT      =   0


}
