package inferenceengine.struct

import java.io.PrintWriter

import edu.arizona.sista.struct.{Counter, Lexicon}
import util.Profiler
import explanationgraph.{CombinationIterator, TableRow, TableStore}
import inferenceengine.CombinationIteratorZeroAware
import inferenceengine.iml.constraint.{InfPatConstraintFinder, VariableList}
import inferenceengine.iml.model._
import inferenceengine.iml.runtime.{Interpreter, TimeLimit}
import inferenceengine.iml.states.InferenceState

import scala.collection.mutable
//import inferenceengine.struct.InferencePattern.interpreter
import inferenceengine.util.TaxonomicCompletion
import inferenceengine.util.TaxonomicCompletion.findCandidateRows
import util.LexiconUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

/**
  * Storage class for an inference pattern in the inference engine
  * Created by peter on 6/19/18.
  */
class InferencePattern(
                        // A name for this inference pattern
                        val name: String,

                        // Plain text description of this inference pattern
                        val description: String,
                        val patternmatchDescription: Expr,

                        // Execution mode (e.g. normal, auto-running, etc)
                        val executionMode: Int,

                        // Instance requirements
                        val instanceRequirements: InfInstanceReq,

                        // Generic constraints, which may also include constraints for composite patterns, but should *not* include instance constraints
                        val constraints: Array[InfConstraint],

                        // Composite inference pattern requirements
                        val compInfReq: Option[CompInfPatReq],

                        // Specific rows
                        val rowsSpecific: Array[TableRow],                      // Unused?

                        // Patterns for rows
                        val rowPatterns: Array[TableRowPattern],

                        // Rules
                        val codeblock: List[Statement],

                        // Tablestore
                        val tablestore:TableStore

                      ) {

  // List of variables in this pattern
  val variableList = getListOfVariables()

  // Candidate rows for patterns
  val rowPatternMatches = new Array[Array[PatternMatchRowFlex]](rowPatterns.size)

  // Full pattern matches
  var fullPatternMatches = new ArrayBuffer[PatternMatchInfPat]

  // Population
  var hasBeenPopulated: Boolean = false

  // Faux interpreter for constraint matching
  val fauxInterpreter = initializeFauxInterpreter(tablestore)


  /*
   * Faux interpreter
   */

  def initializeFauxInterpreter(tablestore: TableStore):Interpreter = {
    val fauxInt = new Interpreter(tablestore)
    // Because this is a 'faux' interpreter, do not automatically add executed patterns to the statespace, so as to avoid contamination issues
    // when the masterInterpreter statespace is shared with this faux interpreter.
    fauxInt.setAutoAddToStateSpace(false)

    // Return
    fauxInt
  }



  /*
   * Population marking
   */
  def isPopulated: Boolean = {
    hasBeenPopulated
  }

  def setPopulated() {
    hasBeenPopulated = true
  }

  def clearPopulated() {
    hasBeenPopulated = false
  }


  /*
   * Pattern matching
   */
  def findPatterns(tablestore: TableStore, maxMatches:Int, useThreads:Boolean = false): Unit = {
    findRowPatternCandidates(tablestore, useThreads)
    //##findPatternsWithConstraints(tablestore)
    findPatternsWithConstraintsFast(tablestore, maxMatches)
  }

  // TODO: In progress
  def patternMatchingComposite(inferencePatterns: ArrayBuffer[InferencePattern], tablestore: TableStore): Unit = {
    if (isCompositePattern()) {
      val success = createInfPatPermutations(inferencePatterns, tablestore, tablestore.lexicon)
    }
  }

  def findRowPatternCandidates(tablestore: TableStore, useThreads:Boolean = false): Unit = {
    // Step 1: For a given row pattern, find all single rows that meet the local constraints of the rows
    if (!useThreads) {
      // Serial
      for (i <- 0 until rowPatterns.length) { // Serial (for debugging)
        rowPatternMatches(i) = rowPatterns(i).getTableRowsMatchingConstraints(tablestore, variableList)
      }
    } else {
      // Parallel
      for (i <- (0 until rowPatterns.length).par) { // Parallel (for running)
        rowPatternMatches(i) = rowPatterns(i).getTableRowsMatchingConstraints(tablestore, variableList)
      }
    }






  }



  //##
  def findRowsSharedConstraints(tablestore:TableStore):String = {
    val os = new StringBuilder()
    val numRows = rowPatterns.length
    val dependencies = new Array[Array[Int]](numRows)

    val startTime = System.nanoTime()
    for (i <- 0 until numRows) {
      val out = new ArrayBuffer[Int]
      for (j <- (i+1) until numRows) {
        if (i != j) {
          //val (dependencies, validConnections) = findRowCandidatesWithOverlap(rowPatterns(i), rowPatterns(j), tablestore, Array(10, 11, 12, 13, 15, 17, 19, 20, 22))
          val (dependencies, validConnections) = findRowCandidatesWithOverlap(rowPatterns(i), rowPatterns(j), tablestore)
          if (dependencies.length > 0) out.append(j)
        }
      }
      os.append("Row " + i + " depends on rows " + out.mkString(",") + "\n")
    }
    val endTime = System.nanoTime()
    val deltaTime = endTime - startTime
    println ("Runtime (findRowsSharedConstraints): " + deltaTime/1000 + " uSec")

    // Return
    os.toString
  }

  //##
  // Create a (coarse but fast) shortlist of rows that may meet constraints.
  // Uses the look-up tables in the TableStore to quickly find cases where one table row might have lexical overlap with another.
  // rows1ToCheck allows specifiying only specific rows from Table1 that will be checked, for speed, if it's know that only those rows are of interest.
  // 'dependencies' comes from the output of findRowsSharedConstraints
  def findRowCandidatesWithOverlap(rp1:TableRowPattern, rp2:TableRowPattern, tablestore:TableStore, rows1ToCheck:Array[Int] = Array.empty[Int]):(Array[(String, Array[String], Array[String])], Array[(Int, Int)]) = {
    // Step 1: Determine what variables (if any) are shared between these two TableRowPatterns
    val dependencies = rp1.findSharedVarConstraints(rp2)
    // If no variables are shared, there is no edge here -- exit
    if (dependencies.size == 0) return (dependencies, Array.empty[(Int, Int)])

    // Step 2: Convert table names to table references
    // TODO: Currently only handles cases with a single table name mentioned
    if ((rp1.tableNames.length > 1) || (rp2.tableNames.length > 1)) throw new RuntimeException("findRowCandidatesWithOverlap: RowPattern found with more than one table name listed")
    val tableName1 = rp1.tableNames(0)
    val tableName2 = rp2.tableNames(0)
    val table1 = tablestore.getTableByName(tableName1).get
    val table2 = tablestore.getTableByName(tableName2).get

    var count:Int = 0
    val startTime = System.nanoTime()

    // Step 3: For each variable shared between the TableRowPatterns
    val out = new ArrayBuffer[(Int, Int)]
    for (i <- 0 until dependencies.length) {
      val dependency = dependencies(i)
      val varName = dependency._1
      val colNames1 = dependency._2
      val colNames2 = dependency._3

      // Step 4: Convert column names to column indices
      val colIds1 = mutable.Set[Int]()
      val colIds2 = mutable.Set[Int]()
      for (colName <- colNames1) {
        val colIdx = table1.getColumnIdx(colName).getOrElse(-1)
        if (colIdx == -1) return (dependencies, Array.empty[(Int, Int)])
        colIds1.add(colIdx)
      }
      for (colName <- colNames2) {
        val colIdx = table2.getColumnIdx(colName).getOrElse(-1)
        if (colIdx == -1) return (dependencies, Array.empty[(Int, Int)])
        colIds2.add(colIdx)
      }

      // Step 5: For each row, grab a list of lexicon IDs in the column(s) used by the variable
      var row1Idx:Int = 0
      var r1CheckIdx:Int = 0
      if (!rows1ToCheck.isEmpty) row1Idx = rows1ToCheck(0)

      breakable {
        while (row1Idx < table1.numRows()) {
          //for (row1Idx <- 0 until table1.numRows()) {

          val row1 = table1.getRowByIdx(row1Idx)
          // Step 1: Find a list of lexicon IDs in this column
          val lexiconIds1 = mutable.Set[Int]()
          for (colIdx <- colIds1) {
            //lexiconIds1 ++= row1.getAllLexiconIDsCol(colIdx)      //## TODO: Only use lemmas?
            lexiconIds1 ++= row1.getCellLemmasAlt(colIdx, alternative = 0, onlyContentTags = true) //## TODO: Only use lemmas?
          }

          // Step 6: Find a list of rows in table2 that have some overlap
          val overlapingRowIdxs = mutable.Set[Int]()
          for (lexiconID <- lexiconIds1) {
            for (colIdx <- colIds2) {
              val rowIdxsWithOverlap = table2.findRowsWithLexiconID(lexiconID, colIdx)
              overlapingRowIdxs ++= rowIdxsWithOverlap
            }
          }

          // Step 7: Make a list of tuples of overlapping rows
          for (row2Idx <- overlapingRowIdxs) {
            out.append((row1Idx, row2Idx))
            count += 1
          }

          // Iterate
          if (rows1ToCheck.isEmpty) {
            // Normal mode (iterate through all rows)
            row1Idx += 1
          } else {
            // Discrete mode (iterate only through specific rows)
            r1CheckIdx += 1
            if (r1CheckIdx >= rows1ToCheck.length) break()
            row1Idx = rows1ToCheck(r1CheckIdx)
          }

        }
      }

      /*
      // Store, and repeat for next variable pattern
      println ("Var " + varName)
      for (a <- 0 until out.length) {
        println ("\t" + out(a))
      }
       */

    }
    val endTime = System.nanoTime()
    val deltaTime = endTime - startTime
    println ("Runtime: " + deltaTime/1000 + " uSec")
    println ("count = " + count)

    // Return
    (dependencies, out.toArray)
  }


  // Return a list of names for each of the rowpatterns
  def getTableRowSlotNames(): Array[String] = {
    val out = new Array[String](rowPatterns.length)

    for (i <- 0 until rowPatterns.length) {
      out(i) = rowPatterns(i).name
    }

    // Return
    out
  }

  /*
  //## Old method
  def findPatternsWithConstraints(tablestore: TableStore) = {
    println("* findPatternsWithConstraints: started... (inference pattern: " + name + ")")

    // Step 1: Across ALL row patterns, find the collections of single rows that combine to meet the global constraints.
    val out = new ArrayBuffer[PatternMatchInfPat]()
    val numPatterns = rowPatterns.length

    println ("Step 1A: Creating new PatternMatches")
    // Step 1A: Edge case where there are zero rows (likely a composite pattern composed entirely of other patterns)
    if (numPatterns == 0) {
      //out.append( new PatternMatchInfPat(0, Array.empty[String], this) )
      out.append(new PatternMatchInfPat(tablestore.lexicon, this))
    }



    // Determine the number of candidates for each pattern match
    val numCandidatesPerPattern = Array.fill[Int](numPatterns)(0)
    for (i <- 0 until numPatterns) {
      numCandidatesPerPattern(i) = rowPatternMatches(i).length
    }

    println ("Step 2: Creating new combinationIterator")
    // Create iterator
    println ("numCandidatesPerPattern: " + numCandidatesPerPattern.toList.toString)
    val iter = new CombinationIteratorZeroAware(numCandidatesPerPattern)

    println ("Step 3: Iterating (size = " + iter.size + ")")
    while (iter.hasNext()) {
      //## Profiler.start("iter.next")
      val indicies = iter.next()
      //## Profiler.end("iter.next")

      //## println (indicies.toList)

      // Create new inference pattern match template
      //## Profiler.start("newPM")
      val pmInfPat = new PatternMatchInfPat(numPatterns, getTableRowSlotNames, this)
      //## Profiler.end("newPM")

      //## Profiler.start("populate")
      // Populate the pattern match elements for this iteration
      for (i <- 0 until indicies.length) {
        val index = indicies(i)
        if (index > -1) {
          pmInfPat.populateSlot(i, rowPatternMatches(i)(index))
        }
      }
      //## Profiler.end("populate")

      //## Profiler.start("checkfully")
      // Check to see whether this is a fully-populated pattern
      if (pmInfPat.isFullyPopulated) {
        // Check to see whether this is a valid pattern (e.g. the variables link up)
        //## Profiler.start("checkvariablesmatch")
        if (pmInfPat.checkVariablesMatch) {
          out.append(pmInfPat)
        }
        //## Profiler.end("checkvariablesmatch")
      }
      //## Profiler.end("checkfully")

      if (iter.count % 500000 == 0) println (Profiler.getReportString(sorted = true))
    }
    //## println ("Profiler Report")
    //## println (Profiler.getReportString(sorted = true))
    //## println ("Step 4: Finished iterating -- returning")

    // Store matches
    fullPatternMatches = out
  }
  */

  // Newer, uses InfPatConstraintFinder to quickly find a shortlist of candidates.
  def findPatternsWithConstraintsFast(tablestore: TableStore, maxMatches:Int) = {
    println("* findPatternsWithConstraints: started... (inference pattern: " + name + ")")

    // Step 1: Across ALL row patterns, find the collections of single rows that combine to meet the global constraints.
    val out = new ArrayBuffer[PatternMatchInfPat]()
    val numPatterns = rowPatterns.length

    println ("Step 1A: Creating new PatternMatches")
    // Step 1A: Edge case where there are zero rows (likely a composite pattern composed entirely of other patterns)
    if (numPatterns == 0) {
      //out.append( new PatternMatchInfPat(0, Array.empty[String], this) )
      out.append(new PatternMatchInfPat(this))
    }

    /*
    // Determine the number of candidates for each pattern match
    val numCandidatesPerPattern = Array.fill[Int](numPatterns)(0)
    for (i <- 0 until numPatterns) {
      numCandidatesPerPattern(i) = rowPatternMatches(i).length
    }

    println ("Step 2: Creating new combinationIterator")
    // Create iterator
    println ("numCandidatesPerPattern: " + numCandidatesPerPattern.toList.toString)
    val iter = new CombinationIteratorZeroAware(numCandidatesPerPattern)
     */

    // Step N: Call the InfPatConstraintFinder, which performs a relatively quick first-pass search for candidates.
    //## Profiler.start("InfPatConstraintFinder")
    println ("Finding combinations")
    val patternMatchCandidates = new InfPatConstraintFinder(this, tablestore, maxMatches)
    //val validCombos = patternMatchCandidates.getValidCombos()
    val validMatches = patternMatchCandidates.validMatches

    val cachedHashcodes = Array.fill[String](validMatches.size)("")
    val hasHashcodeBeenUsed = new mutable.HashMap[String, ArrayBuffer[Int]]()

    //## Profiler.end("InfPatConstraintFinder")

    println ("Step 3: Iterating (size = " + validMatches.length + ")")
    val outTemp = Array.fill[Option[PatternMatchInfPat]](validMatches.length)(None)

    for (comboIdx <- (0 until validMatches.size)) {

      // Create new inference pattern match template
      //##Profiler.start("newPM")
      val pmInfPat = new PatternMatchInfPat(numPatterns, getTableRowSlotNames, this)
      //##Profiler.end("newPM")

      //##Profiler.start("populate")
      // Populate the pattern match elements for this iteration

      val pmrs = validMatches(comboIdx)._1
      val variableLUT = validMatches(comboIdx)._2

      // Populate row slots
      for (rowIdx <- 0 until rowPatterns.length) {
        if (pmrs(rowIdx).isDefined) {
          pmInfPat.populateSlot(rowIdx, pmrs(rowIdx).get)
        }
      }
      // Populate variable LUT
      pmInfPat.addVariables(variableLUT)
      pmInfPat.addRowUIDsToVariableLUT()

      //##Profiler.end("populate")

      //##Profiler.start("checkfully")
      // Check to see whether this is a fully-populated pattern
      if (pmInfPat.isFullyPopulated) {
        // Check to see whether this is a valid pattern (e.g. the variables link up)
        //## Profiler.start("checkvariablesmatch")

        /*
        // TODO: Removed this, as variables are now populated externally. This may have consequences to composite pattern matching.
        if (pmInfPat.checkVariablesMatch) {
          out.append(pmInfPat)
        }
        */


        // Check for possible duplicates
        breakable {
          val thisHashcode = pmInfPat.getHashcode()

          //## Profiler.start("duplicatecheck")
          if (hasHashcodeBeenUsed.contains(thisHashcode)) {   //  Quick check -- if the hashcode hasn't been observed before, then we can skip the expensive checking
            //for (patIdx <- 0 until out.length) {
            val indices = hasHashcodeBeenUsed.get(thisHashcode).get
            //for (patIdx <- 0 until out.length) {
            for (patIdx <- indices) {
              //## println ("Checking Pattern: " + name + " (" + pmInfPat.getHashcode() + ") vs " + patIdx + " (" + outTemp(patIdx).get.getHashcode() + ")")
              // Check if hashcodes are the same
              //## Profiler.start("duplicatecheck1")
              if ((thisHashcode != "0") && (cachedHashcodes(patIdx) == thisHashcode)) {
                // Check if variable lists are the same
                //## Profiler.start("duplicatecheck2")
                if (outTemp(patIdx).get.variableValueLUT.isIdenticalTo(pmInfPat.variableValueLUT)) {
                  //## println("Duplicate pattern found (" + name + " : " + pmInfPat.getHashcode() + "). Pattern will not be added.")
                  // Pattern is likely a duplicate -- do not add.
                  //sys.exit(1)
                  break()
                }
                //## Profiler.end("duplicatecheck2")
              }
              //## Profiler.end("duplicatecheck1")
            }

          }
          //## Profiler.end("duplicatecheck")

          // Pattern is unlikely to be a duplicate -- add
          outTemp(comboIdx) = Some(pmInfPat)
          cachedHashcodes(comboIdx) = thisHashcode      // Store the hashcode for the pattern, since this takes quite a bit of time to generate (~3uSec)
          val indices = hasHashcodeBeenUsed.get(thisHashcode).getOrElse(new ArrayBuffer[Int])
          //indices.append(cachedHashcodes.length-1)
          indices.append(comboIdx)
          hasHashcodeBeenUsed(thisHashcode) = indices
        }


        //## Profiler.end("checkvariablesmatch")
      }
      //##Profiler.end("checkfully")

      //## if (comboIdx % 500 == 0) println (Profiler.getReportString(sorted = true))

      if (comboIdx % 10000 == 0) {
        print ("  " + (comboIdx.toDouble * 100 / validMatches.size.toDouble).formatted("%3.0f") + "%")
        if ((comboIdx % (10000 * 20) == 0) && (comboIdx != 0)) println("")
      }

      // Break if the hard time limit has elapsed.
      if (TimeLimit.isTimeElapsed()) {
        println ("User-specified time limit exceeded (" + TimeLimit.timeLimit + " msec).  Exiting")
        sys.exit(1)
      }
    }

    println ("Step 3A: Reducing... ")
    for (i <- 0 until outTemp.length) {
      if (outTemp(i).isDefined) out.append(outTemp(i).get)
    }

    println ("")
    println ("Step 4: Sorting... ")

    /*
    println ("Profiler Report")
    println (Profiler.getReportString(sorted = true))
    println ("Step 4: Finished iterating -- returning")
     */


    // Store matches
    fullPatternMatches = out.sortBy(r => (r.getNumOptionalUnpopulated(), r.getRelaxedCountDynamicRowsOnly(), r.getRelaxedCountTotal()) )      // Sort by number of constraints that were relaxed -- i.e. best matches (strictest constraints) first

    println ("Step 5: Sorting completed... ")
  }

  /*
   * Checking Inference Pattern requirements
   */

  // Returns the number of instances required for this pattern
  def getNumInstancesRequired(): Int = {
    instanceRequirements.instances.length
  }

  // Returns a list of all instance names used in this pattern
  def getInstanceNames(): Array[String] = {
    val out = new ArrayBuffer[String]

    for (inst <- instanceRequirements.instances) {
      out.append(inst.name)
    }

    // Return
    out.toArray
  }



  /*
   * Refactored constraint evaluation
   */


  // Evaluates whether a given set of constraints can be met by a given PatternMatchInfPat and set of instances.
  // Input:
  //  patternMatch: a reference to a patternMatch to be evaluated.
  //  constraintEvalIdx: a reference to the specific constraint evaluation in this patternMatch to be evaluated.
  //  lexicon: lexicon refeence
  def evaluateConstraintsPattern(patternMatch:PatternMatchInfPat, constraintEvalIdx:Int, variableLUT:Option[VariableValueLUT], tablestore:TableStore, lexicon:Lexicon[String]) {
    // Reference to constraint evaluation that we'll be working with during execution
    val overallConstraintEval = patternMatch.constraintEvals(constraintEvalIdx)

    // Step 1: Get reference to Interpreter instance for verifying conditions
    val interpreter = fauxInterpreter //## InferencePattern.interpreter
    interpreter.resetStates()

    // Step 2: Setup faux instance/state with combination of instances to be tested to see if they meet this pattern's requirements
    // Step 2A: Create new faux state with only the instances in this constraintEval
    val state = new InferenceState

    // Step 2B: Add each instance into the faux state
    for (instIdx <- 0 until overallConstraintEval.instanceMaps.length) {
      // Grab a shorter reference to the instancemap
      val instanceMap = overallConstraintEval.instanceMaps(instIdx)
      // Grab a reference to the real instance
      val instanceRef = instanceMap.existingInstance
      // Find the name that the instance should be called in this pattern
      val instanceNameInPattern = instanceMap.patternInstanceName
      // Clone a copy of the real instance, and rename it the name it is refered to in this pattern
      val fauxInstance = instanceRef.cloneShallow( instanceNameInPattern )

      // Check kindOf requirements while here.  Note that this could be moved to another location.
      checkInstanceKindOfRequirements(instanceMap, instanceRequirements.instances(instIdx), tablestore, lexicon)

      // Add this faux instance to the state
      state.addInstance(fauxInstance)
    }

    // Step 2C: Set interpreter state to faux state
    interpreter.states.addManualState(state)


    // Step 3: Evaluate pattern constraints
    for (constIdx <- 0 until overallConstraintEval.numConstraints()) {
      val constraintEval = overallConstraintEval.getConstraintEval(constIdx)
      val constraint = constraintEval.constraint

      //## println("Constraint: " + constraint)
      //## println("ConstraintEvalHashCode: " + constraintEval.hashCode())

      // Evaluate the constraint, if it hasn't already been processed
      if (!constraintEval.isProcessed()) {
        //## println (" Processing... ")
        // NOTE: Currently InfConstraintMustHave and InfConstraintShouldHave are processed separately, even though the code is identical, as the conditional expression is stored in the subclass rather than superclass.
        constraint match {
          case c: InfConstraintMustHaveOrOmit => processConstraintCondExpr(constraintEval, c.condExpr, variableLUT, interpreter)
          case c: InfConstraintMustHave => processConstraintCondExpr(constraintEval, c.condExpr, variableLUT, interpreter)
          case c: InfConstraintShouldHave => processConstraintCondExpr(constraintEval, c.condExpr, variableLUT, interpreter)
        }
      } else {
        //## println (" Already processed... ")
      }

    }

    // Step 4: Clear interpreter state
    interpreter.resetStates()

  }

  // Helper for above
  def evaluateAllConstraintsPattern(patternMatch:PatternMatchInfPat, variableLUT:Option[VariableValueLUT], tablestore:TableStore, lexicon:Lexicon[String]): Unit = {
    for (constEvalIdx <- 0 until patternMatch.constraintEvals.length) {
      //##
      //println ("constraintEvalIdx: " + constEvalIdx)
      evaluateConstraintsPattern(patternMatch, constEvalIdx, variableLUT, tablestore, lexicon)
    }
  }


  // Process a conditional expression attached to a constraint/constraint evaluation
  // Helper for evaluateConstraints
  def processConstraintCondExpr(constraintEval:ConstraintEval, condExpr:ConditionExpr, variableLUT:Option[VariableValueLUT], interpreter:Interpreter): Unit = {
    val debugOutput:Boolean = false

    //println (" * processConstraintCondExpr(): started... ")
    val (result, success) = interpreter.calculateConditionExpr(condExpr, variableLUT)
    //println ("\t" + condExpr.toString)
    //println ("\tresult: " + result + "  success: " + success)

    if (success) {
      // Set that this constraint has been processed successfully
      constraintEval.setProcessed()

      if (result) {
        // Constraint has been successfully met
        constraintEval.setConstraintMet()
      } else {
        // Constraint has not been met
        constraintEval.setConstraintUnmet()
      }
    } else {
      // This constraint can not be processed successfully
      constraintEval.setUnprocessed()

      if (debugOutput) {
        println("ERROR: Could not evaluate constraint requirement expression: " + condExpr.toString)
        println("LastErrorString: " + interpreter.lastErrorStr)
      }

    }

  }

  // Check whether the KindOf requirements for instance mapping are met.
  // Helper for evaluateConstraints
  def checkInstanceKindOfRequirements(instEval:InstanceMapEval, requirements:InstanceRequirement, tablestore:TableStore, lexicon:Lexicon[String]) {
    // NOTE: KindOf types are stored as lexicon references.
    requirements.populateLexiconRefs(lexicon)

    // Check whether required 'kindof' is identical to the 'kindof' in the current instance evaluation.
    // TODO: This currently uses direct matching, and doesn't currently capture taxonomic allowances -- e.g. if it requires "substance", accepting kindof types that are kinds-of substances.

    // Case 1: Check for direct match (inexpensive)
    if (requirements.kindofLexicon.deep == instEval.existingInstance.kindof.deep) {
      instEval.setKindOfMet()
    } else if (requirements.kindofLexicon.deep == Array(lexicon.add("thing")).deep) {
      // Case 2: Check whether the match is for a kind of 'thing' -- this is a wildcard, and is always true.
      instEval.setKindOfMet()
    } else if (findXKindofYHelper(instEval.existingInstance.kindof, requirements.kindofLexicon, tablestore)) {
      // Case 3: Check for indirect (X is a kind of Y) link  (more costly)
      //## println ("X is a kind of Y!")
      instEval.setKindOfMet()
    } else {
      instEval.setKindOfUnmet()
    }

  }

  // Checks to see if there is a row in the Tablestore that says 'X is a kind of Y'
  // Used for checking constraint requirements for inference patterns.
  def findXKindofYHelper(xTerms:Array[Int], yTerms:Array[Int], tablestore:TableStore):Boolean = {
    val table = tablestore.getTableByName("KINDOF").get
    val hypernymColIdx = table.getColumnIdx("HYPERNYM").get
    val hyponymColIdx = table.getColumnIdx("HYPONYM").get

    // Find rows with all X terms
    val candidateRowsX = TaxonomicCompletion.findCandidateRowsIndicies(xTerms, table, hyponymColIdx)
    // Find rows with all Y terms
    val candidateRowsY = TaxonomicCompletion.findCandidateRowsIndicies(yTerms, table, hypernymColIdx)

    // Calculate rows that contain both terms
    val overlap = candidateRowsX.intersect(candidateRowsY)
    if (overlap.size == 0) {
      // If no overlap, then there can't be a row of the form (X is a kind of Y)
      //## println ("candidateRowsX: " + candidateRowsX)
      //## println ("candidateRowsY: " + candidateRowsY)
      //## println ("**### " + LexiconUtil.lexiconIdxsToStr(xTerms, tablestore.lexicon) + " is not a kind of " + LexiconUtil.lexiconIdxsToStr(yTerms, tablestore.lexicon))
      return false
    }

    // For each possible match
    for (candidateMatchIdx <- overlap) {
      breakable {
        // Get row
        val row = table.getRowByIdx(candidateMatchIdx)

        // Check X column for match
        val xCol = row.getCellWordsAlt(hyponymColIdx, 0)
        val xCol1 = row.getCellLemmasAlt(hyponymColIdx, 0, onlyContentTags = true)
        if ((xCol.deep == xTerms.deep) || (xCol1.deep == xTerms.deep)) {
          // match
        } else {
          break()
        }

        // Check Y column for match
        val yCol = row.getCellWordsAlt(hypernymColIdx, 0)
        val yCol1 = row.getCellLemmasAlt(hypernymColIdx, 0, onlyContentTags = true)
        if ((yCol.deep == yTerms.deep) || (yCol1.deep == yTerms.deep)) {
          // match
        } else {
          break()
        }

        // If we reach here, a match was found
        //## println ("*#*# " + LexiconUtil.lexiconIdxsToStr(xTerms, tablestore.lexicon) + " is a kind of " + LexiconUtil.lexiconIdxsToStr(yTerms, tablestore.lexicon))
        return true
      }
    }

    // If we reach here, no match was found
    //## println ("**## " + LexiconUtil.lexiconIdxsToStr(xTerms, tablestore.lexicon) + " is not a kind of " + LexiconUtil.lexiconIdxsToStr(yTerms, tablestore.lexicon))
    return false
  }


  // The constraint evaluation checks general constraints, which can include constraints that reference instances.
  // To evaluate which of these are true, we have to generate a list of all possible instance mappings.
  // This function generates those mappings, stored in 'InstanceMapEval' storage classes.
  // The 'Eval' is actually quite light in these storage classess -- only checking whether the KINDOF instance mapping requirements are met.
  // Those kindof requirements are not currently evaluated here -- the instance evaluations are blank.
  def mkBlankInstanceEvals(in:Array[ObjectInstance]):Array[Array[InstanceMapEval]] = {
    val out = new ArrayBuffer[ Array[InstanceMapEval] ]
    val numInstancesRequired = instanceRequirements.instances.length
    val numInstancesInStateSpace = in.length

    println ("* mkBlankInstanceEvals: started... ")

    // Step 0: Check whether any instance requirements exist.  This will save a great deal of computation for those without instance requirements.
    if (numInstancesRequired == 0) return (Array.empty[Array[InstanceMapEval]])

    // Step 1: Create an iterator that will generate all possible combinations of instances required for this pattern.
    val maxIter = Array.fill[Int](numInstancesRequired)(numInstancesInStateSpace)
    val iter = new CombinationIterator(maxIter)

    // Step 2: Assemble and pack InstanceMapEval mappings/evaluations for each combination
    while (iter.hasNext()) {
      val instIndices = iter.next() // Get the next combination
      if (!checkForDuplicates(instIndices)) { // Check for duplicates -- only patterns without duplicate instances should be tested

        // Create an array with only the instances being tested, in order
        val instanceCombo = instIndices collect in

        // Pack into InstanceMapEval storage classes.
        val evalsOneSet = new Array[InstanceMapEval](numInstancesRequired)
        for (instIdx <- 0 until numInstancesRequired) {
          val existingInstance = instanceCombo(instIdx)
          val existingInstanceName = existingInstance.name
          val patternInstanceName = instanceRequirements.instances(instIdx).name

          // TODO: Check kindof requirements here? (currently checked each time in evaluateConstraints)
          val kindofMet:Boolean = false

          evalsOneSet(instIdx) = new InstanceMapEval(existingInstanceName, patternInstanceName, existingInstance, kindofMet)
        }

        // Add this set of blank instance map evaluations to the output set
        out.append( evalsOneSet )
      }
    }

    //## Debug display
    /*
    println(" * mkBlankInstanceEvals generated " + out.length + " combinations. ")
    for (i <- 0 until out.length) {
      println("\t" + i + ":(" + out.mkString(", ") + ")")
    }
    */

    // Return
    out.toArray
  }

  // Ensure that there are no duplicate elements in the combination
  // Helper for mkBlankInstanceEvals
  private def checkForDuplicates(in: Array[Int]): Boolean = {
    for (i <- 0 until in.length) {
      for (j <- i + 1 until in.length) {
        if (in(i) == in(j)) return true
      }
    }
    // Return
    false
  }


  // For each PatternMatch, create a set of blank constraint evaluations
  def mkBlankConstraintEvals(in:Array[ObjectInstance]): Unit = {

    // Step 1: Make blank instance mappings (which also contain light kindof evaluations, that are not populated here)
    val instMaps = mkBlankInstanceEvals(in)

    // Step 2: For each of those possible instance evaluations, make blank constraint evaluations
    if (getNumInstancesRequired() > 0) {
      // Instances are required for this pattern
      for (instMap <- instMaps) {
        for (patternMatch <- fullPatternMatches) {
          patternMatch.addUnpopulatedConstraints( instMap )
        }
      }

    } else {
      // No instances are required for this pattern
      val instMap = Array.empty[InstanceMapEval]      // Empty instance mapping

      for (patternMatch <- fullPatternMatches) {
        patternMatch.addUnpopulatedConstraints( instMap )
      }
    }

  }


  /*
   * Main constraint evaluation/population entry points/control functions.
   */
  // Populates blank constraint matching evaluations
  def populateConstraints(in:Array[ObjectInstance]) {
    // Step 1: Clear any previous constraint matching/evaluation
    for (patternMatch <- fullPatternMatches) {
      patternMatch.clearConstraintEvals()
    }

    // Step 2: Make blank constraint evaluations (including instance mappings) for all PatternMatches in this inferencepattern.
    mkBlankConstraintEvals(in)

  }

  // Evaluates constraints for each pattern match in the inference pattern.
  def evaluateConstraints(tablestore:TableStore, lexicon:Lexicon[String]): Unit = {
    // Step 3: Evaluate constraints by evaluating all blank constraint combinations for all PatternMatches in this inference pattern.
    for (patternMatch <- fullPatternMatches) {

      val variableLUT = patternMatch.getVariableLUT()
      evaluateAllConstraintsPattern(patternMatch, Some(variableLUT), tablestore, lexicon)
    }

    // NOTE: PatternMatchInfPat.canConstraintsBeMet() now dynamically determines whether the pattern's constraints can be met at runtime
  }

  // Populate pattern match short description text
  def populatePatternMatchShortDescription(masterInterpreter:Interpreter):Boolean = {
    // Get reference to faux interpreter for pattern evaluation
    val interpreter =fauxInterpreter    //## InferencePattern.interpreter
    interpreter.clearRemap()
    interpreter.resetToGlobalVariableScope()
    interpreter.shareStates(masterInterpreter)


    for (patternMatch <- fullPatternMatches) {

      // Case 1: Pattern has no constraints
      if (patternMatch.constraintEvals.length == 0) {

        val variableLUT = patternMatch.getVariableLUT()

        // Calculate value of expression
        val (valueDouble, valueStr, success) = interpreter.calculateExpr(patternmatchDescription, Some(variableLUT))

        if (!success) {
          println("ERROR: Could not evaluate expression for patternmatch description '" + patternmatchDescription + "'.")
          println ("variableLUT: ")
          println (variableLUT.toStringDelim(delim = "\n"))

          interpreter.clearRemap()
          interpreter.resetToGlobalVariableScope()

          return false
        }

        // Step 2: Retrieve appropriately casted value
        var str: String = ""
        if (valueDouble.isDefined) {
          str = valueDouble.get.toString()
        } else {
          // Otherwise, the value is a string
          str = valueStr.get.toString()
        }

        // Successfully evaluted -- store
        patternMatch.shortDescription = str

      } else {

        // Case 2: Pattern has constriants that may be a part of the short description

        // Step 1: Populate empty short descriptions for each set of constraints for this pattern
        patternMatch.shortDescriptions = Array.fill[String](patternMatch.constraintEvals.length)("No description available")
        //println ("Start")

        //TODO: Clone main interpreter state(s) to get access to instances

        // Step 2: Iterate through each set of constraints
        for (constraintIdx <- 0 until patternMatch.constraintEvals.length) {
          // Step 3A: Clear faux interpreter state
          interpreter.clearRemap()
          interpreter.resetToGlobalVariableScope()

          // Step 3B: Retrieve variableLUT, constraintRemap
          val variableLUT = patternMatch.getVariableLUT()
          val constraintRemap = patternMatch.constraintEvals(constraintIdx).getRemap()
          // TODO: Nested constraints?
          interpreter.addRemapLevel(constraintRemap)

          // Step 3C: Calculate value of expression
          val (valueDouble, valueStr, success) = interpreter.calculateExpr(patternmatchDescription, Some(variableLUT))
          if (!success) {
            println("ERROR: Could not evaluate expression for patternmatch description '" + patternmatchDescription + "'.")
            println ("variableLUT: ")
            println (variableLUT.toStringDelim(delim = "\n"))
            interpreter.clearRemap()
            interpreter.resetToGlobalVariableScope()

            patternMatch.shortDescriptions(constraintIdx) = "Expression could not be evaluated: " + patternmatchDescription

            sys.exit(2)
            //return false
          } else {

            // Step 3D: Retrieve appropriately casted value
            var str: String = ""
            if (valueDouble.isDefined) {
              str = valueDouble.get.toString()
            } else {
              // Otherwise, the value is a string
              str = valueStr.get.toString()
            }

            // Step 3E: Successfully evaluted -- store
            patternMatch.shortDescriptions(constraintIdx) = str
          }

        }

        patternMatch.shortDescription = "Constraint-specific descriptions populated."

      }

    }

    // Clear faux interpreter
    interpreter.clearRemap()
    interpreter.resetToGlobalVariableScope()

    // Stop sharing states -- this must be called after using shareStates(), or future changes will propagate to the original states.
    interpreter.resetStates()

    return true
  }

  // Remove pattern matches that have been flagged with the Omit flag for not meeting critical criteria.
  def removePatternsOmitFlag() {
    var idx = 0
    while (idx < fullPatternMatches.length) {
      if (fullPatternMatches(idx).shouldOmit()) {
        fullPatternMatches.remove(idx)
      } else {
        idx += 1
      }
    }
  }



  /*
   * Composite inference patterns
   */

  // Check to see if this inference pattern is a composite pattern (requiring/merging other inference patterns)
  def isCompositePattern(): Boolean = {
    if (compInfReq.isDefined) {
      if (!compInfReq.get.isEmpty) return true
    }
    // Default return
    false
  }

  // Get the number of inference patterns in a composite inference pattern
  def numInfPatInComposite(): Int = {
    if (!isCompositePattern()) return 0

    // Return
    compInfReq.get.infPatReq.length
  }

  // Get a list of patterns required by this pattern (if composite)
  def getInfPatDependenciesComposite():Array[String] = {
    if (!isCompositePattern()) return Array.empty[String]

    val out = new ArrayBuffer[String]

    for (reqPat <- compInfReq.get.infPatReq) {
      out.append(reqPat.patternName)
    }

    // Return
    out.toArray
  }

  // Check to see if the inference patterns used by this composite pattern have been populated, so that it would be possible to process this pattern.
  def haveRequiredPatternsBeenPopulated(patternsIn: ArrayBuffer[InferencePattern]): Boolean = {
    // Step 1: Get a list of names of patterns required by this pattern
    val requiredNames = mutable.Set[String]()

    for (reqPat <- compInfReq.get.infPatReq) {
      requiredNames += reqPat.patternName
    }

    // Step 2: Check to see if those patterns have already been processed
    var numProcessed: Int = 0
    var numFound = mutable.Set[String]()
    for (i <- 0 until patternsIn.length) {
      if (requiredNames.contains(patternsIn(i).name)) {
        if (!patternsIn(i).isPopulated) {
          return false
        } else {
          numFound += patternsIn(i).name
          numProcessed += 1
        }
      }
    }


    // Step 3: Final check to ensure that all infrence patterns referenced were found in the list of known patterns.
    if (numProcessed == requiredNames.size) {
      return true
    } else {
      val patternsNotFound = requiredNames.diff(numFound)
      println("ERROR: Some patterns were not found in the list of inference patterns (" + patternsNotFound.mkString(", ") + ") for composite pattern '" + name + "'. Composite pattern can not be evaluated. ")
      return false
    }

  }


  // Find a list of permutations of InferencePattern patternMatches that potentially meet the requirements for this composite inference pattern
  def createInfPatPermutations(inferencePatterns:ArrayBuffer[InferencePattern], tablestore:TableStore, lexicon:Lexicon[String]): Boolean = {
    println(" * createInfPatPermutations(): Started... ")

    // Step 0: Get inference pattern requirements for this composite pattern, as a list of (referenceName, InferencePatternName) pairs.
    val infPatReqs = compInfReq.get.infPatReq
    val numRequirements = infPatReqs.length

    // Step 1: Grab a list of patterns that may potentially satisfy the requirements of each inference pattern reference
    val patternsByReq = new Array[Array[PatternMatchInfPat]](numRequirements)
    for (i <- 0 until numRequirements) {
      patternsByReq(i) = getPatterns(infPatReqs(i).patternName, inferencePatterns)
      if (patternsByReq(i).length == 0) {
        // This inference pattern requirement can not be met -- no pattern matches were found for an inference pattern it requires
        fullPatternMatches.clear()
        return false
      }
    }

    // Step 2: Create permutations
    val maxIter = new Array[Int](numRequirements)
    for (i <- 0 until numRequirements) {
      maxIter(i) = patternsByReq(i).length
    }

    val iter = new CombinationIterator(maxIter)

    println("maxIter: " + maxIter.mkString(", "))


    // Pack the permutations into a format that's easier to test
    // For each permutation
    val pmCombos = new ArrayBuffer[Array[PatternMatchInfPat]]
    while (iter.hasNext()) {
      val indicies = iter.next()
      //## println("indices: " + indicies.mkString(", "))

      // NOTE: checkForDuplicates isn't appropriate here -- have to create a separate function that checks for duplicates amoung infpat's that are filled by the same inferencepattern type.
      // BUT: It's not clear whether this is desired behavior -- perhaps it's okay for patterns to reuse the same inference pattern instances, and patterns that don't want this can filter it out in the pattern specification.
      //if (!checkForDuplicates(indicies)) {

      // Create permutation
      val pmCombo = new Array[PatternMatchInfPat](numRequirements)
      for (i <- 0 until numRequirements) {
        pmCombo(i) = patternsByReq(i)(indicies(i))
      }
      // Store permutation
      pmCombos.append(pmCombo)
      //}
    }


    //## DEBUG
    /*
    println("Combos (" + pmCombos.length + "): ")
    for (i <- 0 until pmCombos.length) {
      val pmCombo = pmCombos(i)
      println("Combo " + i + ": \t")

      for (j <- 0 until pmCombo.length) {
        println("Slot " + j + " (" + infPatReqs(j).referenceName + ":" + infPatReqs(j).patternName + "): " + pmCombo(j).toString())
      }
      println("")
    }
    */

    // Get inference pattern reference names
    val infPatRefNames = new Array[String](numRequirements)
    for (i <- 0 until numRequirements) {
      infPatRefNames(i) = infPatReqs(i).referenceName
    }

    // Step 3: Create pattern match candidates for each permutation.  Composite patterns inherit variable LUTs from component inference patterns.
    val patternMatchCandidates = new ArrayBuffer[PatternMatchInfPat]

    if (fullPatternMatches.length > 0) {
      // Case: existing pattern matches on composite pattern's internal rows
      for (existingPM <- fullPatternMatches) {
        for (pmCombo <- pmCombos) {
          val clonedPM = existingPM.clone()
          clonedPM.setCompositePattern(pmCombo, infPatRefNames)
          clonedPM.compositeInheritVariables(lexicon)
          patternMatchCandidates.append(clonedPM)
        }
      }
    } else if (this.rowPatterns.length == 0) {
      // This composite pattern has no table row matches of it's own, so it won't have any patternMatches.  Create a blank patternMatch to populate
      for (pmCombo <- pmCombos) {
        val pmBlank = new PatternMatchInfPat(0, Array.empty[String], this)
        pmBlank.setCompositePattern(pmCombo, infPatRefNames)
        pmBlank.compositeInheritVariables(lexicon)
        patternMatchCandidates.append(pmBlank)
      }
    }


    // Step 4: Check permutations for whether they meet row reference requirements
    var idx: Int = 0
    while (idx < patternMatchCandidates.length) {
      //## println("Check: " + idx)
      if (!patternMatchCandidates(idx).checkCompositeRowRefReq(tablestore)) {
        // Row reference requirements are *not* met for this pattern -- remove it
        patternMatchCandidates.remove(idx)
      } else {
        // Row reference requirements are met for this pattern -- keep it, and continue to check the next pattern
        idx += 1
      }
    }


    // TODO: Step 4: Check if combinations are valid

    // Check permutations for whether they meet instance requirements

    // Check which instances are present in instmaps
    val instancesMapped = mutable.Set[String]()
    for (instEquiv <- compInfReq.get.instEquiv) {
      instancesMapped += instEquiv.patternRefName2 + "." + instEquiv.instName2
    }
    println("Instances mapped: " + instancesMapped.mkString(", "))

    // Check which instances are present in permutations
    println("Check if permuations meet instance requirements (" + name + "):")
    for (i <- 0 until patternMatchCandidates.length) {
      val instanceNames = patternMatchCandidates(i).getInstanceNamesComposite()
      println(i + ": " + instanceNames.mkString(", "))

      val unmappedInstances = instanceNames.diff(instancesMapped) // Missing instances that need to be mapped in the composite pattern
      val extraMappedInstances = instancesMapped.diff(instanceNames) // Extra instances mapped in the composite pattern that don't appear in the component inference patterns
      println("\t Missing unmapped instances: " + unmappedInstances.mkString(", "))
      println("\t Extra mapped instances: " + extraMappedInstances.mkString(", "))


    }


    // Use instance mappings to copy constraints from component inference patterns
    if (patternMatchCandidates.length > 0) {
      for (pmc <- patternMatchCandidates) {
        copyConstraintsFromComponentInfPats(pmc)
      }
    }


    // Re-check any pattern matches that have unprocessed constraints
    println(name)
    println("patternMatchCandidates.length: " + patternMatchCandidates.length)
    for (idx <- 0 until patternMatchCandidates.length) {
      println("idx: " + idx)
      val patternMatch = patternMatchCandidates(idx)

      // Check if this patternMatch has unprocessed instance matches that need rechecking with the updated composite variable LUT.
      if (patternMatch.hasUnprocessedConstraints) {
        println("Has unprocessed...")

        // Re-evaluate constraints, now that the composite variable LUT has been populated
        val variableLUT = patternMatch.getVariableLUT()
        //## println ("VariableLUT: " + variableLUT.toString())
        evaluateAllConstraintsPattern(patternMatch, Some(variableLUT), tablestore, lexicon)


      } else {
        println("No unprocessed...")
      }
    }

    // For composite patterns, sometimes the row LUT is not consistently populated.  Re-check/repopulate this.
    // Note: It's not clear where this is happening, and may be a larger bug that remains undiscovered.
    // Currently it causes duplicate patterns to be generated, with slightly different variable LUTs (one with the rows properly populated, one without).
    // Ensuring this is populated again below is a workaround.
    for (pm <- patternMatchCandidates) pm.addRowUIDsToVariableLUT()

    // Check for possible duplicates
    val filteredNoDup = new ArrayBuffer[PatternMatchInfPat]()
    for (pmCandidate <- patternMatchCandidates) {
      breakable {
        for (pmCheck <- filteredNoDup) {
          //println("Checking Composite Pattern: " + name + " (" + pmCandidate.getHashcode() + ") vs (" + pmCheck.getHashcode() + ")")
          // Check if hashcodes are the same
          if ((pmCandidate.getHashcode() != "0") && (pmCandidate.getHashcode() == pmCheck.getHashcode())) {
            // Check if variable lists are the same
            if (pmCandidate.variableValueLUT.isIdenticalTo(pmCheck.variableValueLUT)) {
              println("Duplicate composite pattern found (" + name + " : " + pmCandidate.getHashcode() + "). Pattern will not be added.")
              // Pattern is likely a duplicate -- do not add.
              //sys.exit(1)
              break()
            }
          }
        }
        // Pattern is unlikely to be a duplicate -- add
        filteredNoDup.append(pmCandidate)
      }
    }

    //## TEST (pre-filtering)
    fullPatternMatches = filteredNoDup


    println(" * createInfPatPermutations(): Completed... ")
    return false
  }


  def copyConstraintsFromComponentInfPats(pm:PatternMatchInfPat) {
    println (" * copyConstraintsFromComponentInfPats: started... ")

    // Group instance mappings by reference
    val instMapByRef = compInfReq.get.instEquiv.groupBy(_.patternRefName2)

    for (componentName <- instMapByRef.keySet) {
      // Step 1: Get combination
      val mapCombination = instMapByRef(componentName)
      //##
      println ("Map Combination (" + componentName + "): " + mapCombination)


      // Step 2: Find the slot index that refers to this component pattern
      val slotIdx = pm.getInfPatCompositeSlotIdx(componentName)
      if (slotIdx < 0) {
        throw new RuntimeException("ERROR: Could not find slot with name '" + componentName + "' in composite pattern '" + name +"'.")
      }
      val componentPattern = pm.getInfPatCompositeInSlot(slotIdx)

      // For each composite constraint evaluation
      for (compositeCEIdx <- 0 until pm.constraintEvals.length) {
        val compositeCE = pm.constraintEvals(compositeCEIdx)

        // Find the one constraint eval in the component pattern that has the same mapping
        breakable {
          for (componentCEIdx <- 0 until componentPattern.constraintEvals.length) {
            val componentCE = componentPattern.constraintEvals(componentCEIdx)
            if (hasSameInstanceMappings(componentCE, compositeCE, mapCombination)) {
              // The mappings are the same, so the instance constraints are the same.  Copy the constraints from the component to the composite
              println ("Merging (compositeCEIdx: " + compositeCEIdx + "   with   componentCEIdx: " + componentCEIdx )

              // NOTE: This only works if the constraints being merged from the component have already been processed -- if they haven't, the references in the constraints will all be out of scope, and unable to process.
              pm.mergeConstraintEvals(compositeCEIdx, componentCE)
              break()
            }
          }

          //TODO:## If we reach here, then a match for the component could not be found -- remove?
          // This would be an unusual case, as it would suggest that the component patterns haven't been fully populated.
          throw new RuntimeException(" * copyConstraintsFromComponentInfPats(): Could not find component constraint evaluation that has the same instance mappings as composite pattern, for composite pattern '" + pm.inferencePattern.name + "'.")
        }

      }

    }

  }

  // Check to see if the constraints in 'component' have the same instance mappings as 'composite'
  def hasSameInstanceMappings(componentCE:OverallConstraintEval, compositeCE:OverallConstraintEval, instMaps:List[CompInstEquivalency]):Boolean = {
    val componentInstMaps = componentCE.instanceMaps
    val compositeInstMaps = compositeCE.instanceMaps

    // Check that each instance in the component is mapped to the appropriate instance in the composite
    for (instMap1 <- componentInstMaps) {
      // Find the name in the component
      val componentInstName = instMap1.patternInstanceName
      val globalNameComponent = instMap1.existingInstanceName

      // Remap the name from composite to component
      var nameInComposite = ""
      for (mapping <- instMaps) {
        if (mapping.instName2 == componentInstName) nameInComposite = mapping.instName1
      }

      // Find the name in the composite
      for (instMap2 <- compositeInstMaps) {
        val compositeInstName = instMap2.patternInstanceName
        val globalNameComposite = instMap2.existingInstanceName
        if (compositeInstName == nameInComposite) {
          if (globalNameComponent != globalNameComposite) {
            // The mapping is not the same
            return false
          }
        }
      }

    }

    // Return
    true
  }




  // Get a map of instmap requirements for composite patterns.  e.g. x.substance1 -> sub1, x.substance2 = sub2, y.substance => sub1.
  // The mapping direction for the names is (nested inference pattern (e.g. x.substance1) -> this (composite) pattern (e.g. sub1) )
  def getInstanceMapComposite(): (Map[String, String], Boolean) = {
    val out = mutable.Map[String, String]()

    for (instEquiv <- compInfReq.get.instEquiv) {
      val mapFrom = instEquiv.patternRefName2 + "." + instEquiv.instName2
      val mapTo = instEquiv.instName1
      if (out.contains(mapFrom)) {
        println("ERROR: getInstanceMapComposite(): Inference pattern already has an instmap to '" + mapFrom + "' from '" + out(mapFrom) + "'.  Can't add another ('" + mapTo + "').")
        return (Map[String, String](), false)
      } else {
        out(mapFrom) = mapTo
      }
    }

    // Return
    (out.toMap, true)
  }


  // Helper for createInfPatPermutations
  // Get all the pattern matches for the inference pattern with name 'name'
  private def getPatterns(name: String, inferencePatterns: ArrayBuffer[InferencePattern]): Array[PatternMatchInfPat] = {
    for (i <- 0 until inferencePatterns.length) {
      if (inferencePatterns(i).name == name) {
        return inferencePatterns(i).fullPatternMatches.toArray
      }
    }

    // Default return
    Array.empty[PatternMatchInfPat]
  }


  /*
   * Supporting functions
   */
  def getListOfVariables():VariableList = {
    // TODO: Note, this finds all variables across all OR options in a pattern. It's possible this may do strange things if some OR patterns have variables that are not present in others.
    val out = new VariableList()

    // Step 1: Find the set of all variable names
    val varNames = mutable.Set[String]()
    for (rowPattern <- rowPatterns) {
      for (cellPattern <- rowPattern.cellPatterns) {
        for (patternOption <- cellPattern.patternOptions) {
          for (elem <- patternOption) {
            if (elem.isVariable) {
              varNames.add(elem.getVariableName)
            }
          }
        }
      }
    }

    // Step 2: Create a look-up table
    for (varName <- varNames) {
      out.addVariable(varName)
    }

    // Return
    out
  }

  /*
   * Summary statistics
   */
  def getSummaryStatistics():Counter[String] = {
    val out = new Counter[String]
    var rowsWithVariables:Int = 0
    var rowsWithoutVariables:Int = 0
    val rowsTotal = rowPatterns.length
    var variableList = mutable.Set[String]()

    for (rowPattern <- rowPatterns) {
      val variables = rowPattern.getVariablesInPattern()
      variableList = variableList ++= variables

      if (rowPattern.hasStaticUUIDs) {
        // Static UUIDs listed, so the interpreter will disregard any other content on this row and just use the static UUID
        rowsWithoutVariables += 1
      } else if (variables.size > 0) {
        // Variables listed -- this row has variables
        rowsWithVariables += 1
      } else {
        // Otherwise -- no static uuids and no variables, so this row is likely all string matching (an unusual case)
        rowsWithoutVariables += 1
      }
    }

    out.setCount("PATTERN_ROWS_WITH_VARS", rowsWithVariables)
    out.setCount("PATTERN_ROWS_WITHOUT_VARS", rowsWithoutVariables)
    out.setCount("PATTERN_ROWS_TOTAL", rowsTotal)
    out.setCount("PATTERN_NUM_UNIQUE_VARIABLES", variableList.size)

    out.setCount("PATTERN_ROWS_WITH_VARS_COUNT", 1.0)
    out.setCount("PATTERN_ROWS_WITHOUT_VARS_COUNT", 1.0)
    out.setCount("PATTERN_ROWS_TOTAL_COUNT", 1.0)
    out.setCount("PATTERN_NUM_UNIQUE_VARIABLES_COUNT", 1.0)

    // Return
    out
  }


  /*
   * String (supporting functions)
   */
  def candidateRowsToString(lexicon: Lexicon[String]): String = {
    val os = new StringBuilder

    for (i <- 0 until rowPatterns.length) {
      os.append("RowPattern " + i + ": \n")
      os.append(rowPatterns(i).toString())
      os.append("\n")

      os.append("Matches: ")
      for (j <- 0 until rowPatternMatches(i).length) {
        os.append(rowPatternMatches(i)(j).toString(lexicon))
        os.append("\n")
      }

    }

    // Return
    os.toString()
  }


  def validInferencePatternsToString(lexicon: Lexicon[String]): String = {
    val os = new StringBuilder

    for (i <- 0 until fullPatternMatches.length) {
      os.append("PatternMatch Inference Pattern (" + description + ") " + i + "\n")
      os.append(fullPatternMatches(i).toString(lexicon) + "\n")
      os.append("\n")
      os.append("<hr>\n")
    }

    // Return
    os.toString
  }

  def validInferencePatternsToHTMLString(lexicon: Lexicon[String], maxPatterns:Int = 200): String = {
    // Generate strings for each pattern
    val patternStrings = new ArrayBuffer[(String, Boolean)]
    var calculateConstraints:Boolean = true
    /*
    if (fullPatternMatches.length > 10000) {
      calculateConstraints = false
      println ("WARNING: Number of pattern matches is very large (" + fullPatternMatches.length + ").  To save time during export, canConstraintsBeMet() will not be calculated.")
    }
    */

    var count:Int = 0

    breakable {
      for (i <- 0 until fullPatternMatches.length) {
        val patternString = new StringBuilder
        var canConstraintsBeMet: Boolean = false
        //## Profiler.start("constraints")
        if (calculateConstraints) canConstraintsBeMet = fullPatternMatches(i).canContraintsBeMet()
        //## Profiler.end("constraints")

        //## Profiler.start("hashcode")
        patternString.append("<b>PatternMatch Inference Pattern (" + description + ") " + i + " (hashcode: " + fullPatternMatches(i).getHashcode() + ") </b> \n")
        //## Profiler.end("hashcode")

        patternString.append("<br><b>Short description:</b> " + fullPatternMatches(i).shortDescription + " \n")
        patternString.append("<br><b>Can constraints be met:</b> ")
        if (calculateConstraints) {
          if (canConstraintsBeMet) {
            patternString.append("<font color=green>true</font> \n")
          } else {
            patternString.append("<font color=red>false</font> \n")
          }
        } else {
          patternString.append("<font color=yellow>too many matches -- not calculated</font> \n")
        }
        //## Profiler.start("tostring")
        patternString.append(fullPatternMatches(i).toHTMLString(lexicon) + "\n")
        //## Profiler.end("tostring")
        patternString.append("\n")
        patternString.append("<hr><br>\n")

        //## Profiler.start("append")
        patternStrings.append((patternString.toString, canConstraintsBeMet))
        //## Profiler.end("append")

        if (i % 10000 == 0) {
          print("  " + (i.toDouble * 100 / fullPatternMatches.length.toDouble).formatted("%3.0f") + "%")
          if ((i % (10000 * 20) == 0) && (i != 0)) println("")

          //## println(Profiler.getReportString())
        }

        // Generating the HTML toString() can be time consuming for very large sets of patterns (10,000+), particularly
        // when most of those patterns are not exported.  Here, if we've collected enough patterns to generate the export,
        // then break early.
        if (calculateConstraints && canConstraintsBeMet) {
          count += 1
        } else if (!calculateConstraints) {
          count += 1
        }
        if (count > maxPatterns) {
          println ("")
          println (" Reached maximum number of patterns (" + maxPatterns + ")")
          break
        }
      }
    }

    // Sort the pattern strings to place those with valid matches first
    val os = new StringBuilder

    if (fullPatternMatches.length > maxPatterns) {
      os.append("<b><font color=\"red\">NOTE: Large number of matches found.  Truncated to " + maxPatterns + ".</b></font><br><br>")
    }

    println ("Sorting")
    val sorted = patternStrings.sortBy(!_._2)

    println ("Exporting Sorting")
    for (i <- 0 until math.min(sorted.size, maxPatterns)) {
      val oneStr = sorted(i)
      os.append(oneStr._1)
    }

    // Return
    os.toString
  }

  /*
   * HTML Export
   */

  // Generates debug HTML showing the rows that match each row slot
  def generateDebugHTML(filename:String, patternName:String, tablestore:TableStore, prefix:String = ""): Unit = {
    val os = new StringBuilder

    println (" * generateHTML: started... (filename = " + filename + ")")

    os.append("<html> \n")
    os.append("<head> \n")
    os.append("<title> Debug Export -- " + patternName + " </title> \n")
    os.append(" <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\"> \n")
    os.append("<style>")
    os.append("table.log, th.log, td.log {\n    border: 1px solid black;\n border-collapse: collapse;\n padding: 5px;\n}")
    os.append("table.log#t01 tr:nth-child(even) {\n    background-color: #eee;\n}\ntable.log#t01 tr:nth-child(odd) {\n    background-color: #fff;\n}\ntable.log#t01 th {\n   background-color: #cce4ff;\n}")
    os.append("</style>")
    os.append("</head> \n")
    os.append("<body> \n")

    // Add the accordion script
    os.append("<script>\n")
    os.append("function accordion(id) { \n")
    os.append("  var x = document.getElementById(id);\n")
    os.append("  if (x.className.indexOf(\"w3-show\") == -1) { \n")
    os.append("    x.className += \" w3-show\"; \n")
    os.append("  } else { \n")
    os.append("    x.className = x.className.replace(\" w3-show\", \"\"); \n")
    os.append("  }\n")
    os.append("} \n")
    os.append("</script>\n")

    os.append(" <div class=\"w3-container\"> \n")

    os.append("<h2>" + patternName + "</h2> \n")

    // Generate HTML
    os.append( debugDisplayRowMatches(tablestore), prefix )


    os.append(" </div> \n")
    os.append("</body> \n")
    os.append("</html> \n")

    // Export
    val pw = new PrintWriter(filename)
    pw.println( os.toString() )
    pw.close()

    println (" * generateHTML: export completed... ")

  }

  // Helper function: Generates HTML for each row slot of debug export
  def debugDisplayRowMatches(tablestore:TableStore, prefix:String = ""):String = {
    val os = new StringBuilder()

    os.append("Row Patterns: " + rowPatterns.length + "<br>\n")

    for (i <- 0 until rowPatterns.length) {
      os.append(rowPatterns(i).getHTMLString(tablestore, variableList, prefix) + "\n")
    }

    // Return
    os.toString()
  }


  /*
   * String
   */
  //override def toString(): String = {
  def toString(tablestore:TableStore): String = {
    val os = new StringBuilder
    os.append("Name: " + name + "\n")
    os.append("Description: " + description + "\n")

    os.append("Instance Requirements: " + instanceRequirements.toString() + "\n")

    os.append("Specific Rows (size=" + rowsSpecific.length + "): \n")
    for (i <- 0 until rowsSpecific.length) {
      os.append(i + ": \t" + rowsSpecific(i).toStringSentWithUID() + "\n")
    }
    os.append("\n")

    os.append("Patterns (size=" + rowPatterns.length + "): \n")
    for (i <- 0 until rowPatterns.length) {
      os.append("* Pattern " + i + ": \n")
      os.append(rowPatterns(i).toString())
      os.append("\n")
    }
    os.append("\n")

    os.append("Row Variable Dependencies: \n")
    //os.append(findRowsSharedConstraints(tablestore) + "\n")
    //##


    //##val constraintGraph = new ConstraintGraphIterator(rowPatterns, tablestore)
    //##os.append(constraintGraph.toString())
    /*
    // debugging
    val infPatConstraintFinder = new InfPatConstraintFinder(this, tablestore)
    os.append(infPatConstraintFinder.toString())
    os.append("\n")
    */

    os.append("Codeblock (size=" + codeblock.length + "): \n")
    for (i <- 0 until codeblock.length) {
      os.append(i + ": \n")
      os.append(codeblock(i).toString())
    }

    // Return
    os.toString()
  }

}


object InferencePattern {
  val VARIABLE_OPT_MARKER = "*"
  val UUID_PREFIX = "UUID"
  val PATMATCH_PREFIX = "INFPATMATCH"

  // Execution modes
  val EXECUTIONMODE_NORMAL      = 0
  val EXECUTIONMODE_AUTO        = 1
  val EXECUTIONMODE_AUTOREGEN   = 2

  val executionModeMap = Map("NORMAL" -> EXECUTIONMODE_NORMAL,
                            "AUTO" -> EXECUTIONMODE_AUTO,
                            "AUTOREGEN" -> EXECUTIONMODE_AUTOREGEN)
  val validExecutionModes = executionModeMap.keySet


  /*
  // Faux Interpreter
  var interpreter: Interpreter = null

  def InitializeInterpreter(tablestore: TableStore): Unit = {
    interpreter = new Interpreter(tablestore)
    // Because this is a 'faux' interpreter, do not automatically add executed patterns to the statespace, so as to avoid contamination issues
    // when the masterInterpreter statespace is shared with this faux interpreter.
    interpreter.setAutoAddToStateSpace(false)
  }
   */

  // Generator
  def mkInferencePattern(name: String, description: String, pmDescription: Expr, executionMode:Int, instanceReq: InfInstanceReq, compInfReq: Option[CompInfPatReq], constraints: Array[InfConstraint], rowsSpecific: Array[TableRow], rowPatterns: Array[TableRowPattern], codeblock: List[Statement], tablestore:TableStore): InferencePattern = {
    new InferencePattern(name, description, pmDescription, executionMode:Int, instanceReq, constraints, compInfReq, rowsSpecific, rowPatterns, codeblock, tablestore)
  }


  // Export (debug)
  // Generates debug HTML showing the rows that match each row slot
  def generateDebugHTMLAllPatterns(filename:String, inferencePatterns:Array[InferencePattern], tablestore:TableStore): Unit = {
    val os = new StringBuilder

    println (" * generateHTML: started... (filename = " + filename + ")")

    os.append("<html> \n")
    os.append("<head> \n")
    os.append("<title> Debug Export -- Inference Pattern Row Matches </title> \n")
    os.append(" <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\"> \n")
    os.append("<style>")
    os.append("table.log, th.log, td.log {\n    border: 1px solid black;\n border-collapse: collapse;\n padding: 5px;\n}")
    os.append("table.log#t01 tr:nth-child(even) {\n    background-color: #eee;\n}\ntable.log#t01 tr:nth-child(odd) {\n    background-color: #fff;\n}\ntable.log#t01 th {\n   background-color: #cce4ff;\n}")
    os.append("</style>")
    os.append("</head> \n")
    os.append("<body> \n")

    // Add the accordion script
    os.append("<script>\n")
    os.append("function accordion(id) { \n")
    os.append("  var x = document.getElementById(id);\n")
    os.append("  if (x.className.indexOf(\"w3-show\") == -1) { \n")
    os.append("    x.className += \" w3-show\"; \n")
    os.append("  } else { \n")
    os.append("    x.className = x.className.replace(\" w3-show\", \"\"); \n")
    os.append("  }\n")
    os.append("} \n")
    os.append("</script>\n")

    os.append(" <div class=\"w3-container\"> \n")

    for (i <- 0 until inferencePatterns.length) {
      os.append("<h2>" + i + ": " + inferencePatterns(i).name + "</h2> \n")

      // Generate HTML
      os.append(inferencePatterns(i).debugDisplayRowMatches(tablestore, prefix = "infpat" + i))

      os.append("<br>\n")
    }


    os.append(" </div> \n")
    os.append("</body> \n")
    os.append("</html> \n")

    // Export
    val pw = new PrintWriter(filename)
    pw.println( os.toString() )
    pw.close()

    println (" * generateHTML: export completed... ")

  }

}