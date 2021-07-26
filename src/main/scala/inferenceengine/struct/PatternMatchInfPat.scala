package inferenceengine.struct

import edu.arizona.sista.struct.Lexicon
import explanationgraph.{CombinationIterator, TableRow, TableStore}
import InferencePattern.{PATMATCH_PREFIX, UUID_PREFIX}
import ConstraintEval._
import edu.stanford.nlp.io.EncodingPrintWriter.out
import util.{LexiconUtil, Profiler}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._


/**
  * Storage class for noting pattern matches across entire inference patterns
  * Created by user on 6/28/18.
  */
class PatternMatchInfPat(val numSlots:Int, rowNames:Array[String], val inferencePattern:InferencePattern) {
  var rowSlots = Array.fill[Option[PatternMatchRow]](numSlots)(None)
  var isPopulated = Array.fill[Boolean](numSlots)(false)

  // assertion checking
  if (rowNames.length != numSlots) throw new RuntimeException("PatternMatchInfPat: ERROR: 'numSlots' is not equal to the length of 'rowNames'. ")

  // Look-up table for variables and values.  Only populated after calling checkVariablesMatch(), and if that call is successful.
  val lexicon = inferencePattern.tablestore.lexicon
  var variableValueLUT = initializeVariableLUT()    //## new VariableValueLUT(lexicon)

  // Evaluating how this pattern meets constraints (musthave/shouldhave)
  var constraintEvals = Array.empty[OverallConstraintEval]

  // Composite patterns: slots for filling the inference pattern requirements
  var infPatSlots = Array.empty[PatternMatchInfPat]

  // Composite patterns: names of references for inference pattern slots
  var infPatRefNames = Array.empty[String]

  // Short description text
  var shortDescription:String = "Not available"
  var shortDescriptions = Array.empty[String]     // Including constraints

  // Alternate Constructor
  def this(infPat:InferencePattern) = {
    this(0, Array.empty[String], infPat)
  }


  /*
   * Variable LUT initialization
   */
  // Initialize all variables to blank values
  def initializeVariableLUT():VariableValueLUT = {
    val out = new VariableValueLUT(lexicon)
    for (varName <- inferencePattern.variableList.getAllVariableNames()) {
      out.setVariable(varName, Array.empty[Int])
    }
    // Return
    out
  }

  def numPopulatedVariables():Int = {
    variableValueLUT.numPopulatedVariables()
  }

  /*
   * Constraints
   */

  // Create a new set of unpopulated constraints in an OverallConstraintEval storage class
  def mkUnpopulatedConstraints(instanceMaps:Array[InstanceMapEval]):OverallConstraintEval = {
    val evals = new ArrayBuffer[ConstraintEval]

    for (constraint <- inferencePattern.constraints) {
      val eval = new ConstraintEval(constraint)
      evals.append( eval )
    }

    // Return
    new OverallConstraintEval(instanceMaps, evals.toArray)
  }

  // Add a new set of unpopulated constraints to evaluate for this pattern match
  def addUnpopulatedConstraints(instanceMaps:Array[InstanceMapEval]): Unit = {
    constraintEvals = constraintEvals ++ Array(mkUnpopulatedConstraints(instanceMaps))
  }

  // Merge evals in OverallContstraintEval -- used specifically for composite patterns, to import constraints from component patterns
  // NOTE: This only works if the constraints being merged have already been processed -- if they haven't, the references in the constraints will all be out of scope, and unable to process.
  def mergeConstraintEvals(constraintEvalIdx:Int, constraintEval:OverallConstraintEval) = {
    val instanceMaps = constraintEvals(constraintEvalIdx).instanceMaps
    val evals = constraintEvals(constraintEvalIdx).evals ++ constraintEval.evals
    constraintEvals(constraintEvalIdx) = new OverallConstraintEval(instanceMaps, evals)
  }

  def clearConstraintEvals(): Unit = {
    constraintEvals = Array.empty[OverallConstraintEval]
  }

  def shouldOmit():Boolean = {
    // Do not omit if there are no constraint evaluations
    if (constraintEvals.length == 0) return false

    // Check if at least one is good
    var countNotOmit:Int = 0
    for (eval <- constraintEvals) {
      if (!eval.shouldOmit()) countNotOmit += 1
    }

    if (countNotOmit == 0) return true

    // Default return
    false
  }


  /*
   * Cloning
   */
  override def clone():PatternMatchInfPat = {
    val out = new PatternMatchInfPat(numSlots, rowNames, inferencePattern)

    for (i <- 0 until rowSlots.length) {
      if (rowSlots(i).isDefined) {
        out.rowSlots(i) = Some(this.rowSlots(i).get.clone())
      }
    }
    out.isPopulated = this.isPopulated.clone()

    // Deep clone Variable Value LUTs
    out.variableValueLUT = this.variableValueLUT.clone()

    // Clone other variables
    out.constraintEvals = this.constraintEvals.map(_.clone())
    out.infPatSlots = this.infPatSlots.map(_.clone())
    out.infPatRefNames = this.infPatRefNames.clone()
    out.shortDescription = this.shortDescription

    // Return
    out
  }

  /*
   * Setters
   */
  def populateSlot(idx:Int, pmRow:PatternMatchRow): Unit = {
    rowSlots(idx) = Some(pmRow)
    isPopulated(idx) = true
  }

  /*
   * Accessors
   */
  def isFullyPopulated:Boolean = {
    for (i <- 0 until isPopulated.length) {
      if (!isPopulated(i) && !inferencePattern.rowPatterns(i).isOptional) return false
    }
    // Default return
    true
  }

  def getRowName(slotIdx:Int):String = {
    rowNames(slotIdx)
  }

  def getRelaxedCountTotal():Int = {
    var sum:Int = 0
    for (rowSlot <- rowSlots) {
      if (rowSlot.isDefined) {
        sum += rowSlot.get.getRelaxedCount()
      }
    }
    sum
  }

  def getRelaxedCountDynamicRowsOnly():Int = {
    var sum:Int = 0
    for (rowSlot <- rowSlots) {
      if (rowSlot.isDefined) {
        if (!rowSlot.get.generatedFromStaticUUID) sum += rowSlot.get.getRelaxedCount()
      }
    }
    sum
  }

  def getNumOptionalUnpopulated():Int = {
    var sum:Int = 0
    for (rowIdx <- 0 until numSlots) {
      if (!isPopulated(rowIdx) && inferencePattern.rowPatterns(rowIdx).isOptional) sum += 1
    }
    // Return
    sum
  }

  /*
   * Composite patterns
   */
  def isComposite:Boolean = {
    if (infPatRefNames.length > 0) return true
    // Otherwise
    return false
  }

  // Set that this is a composite pattern by providing a list of potential fillers for the inference pattern slots, as well as the names of those inference pattern slots.
  def setCompositePattern(_infPatSlots:Array[PatternMatchInfPat], _infPatRefNames:Array[String]) {
    infPatSlots = _infPatSlots
    infPatRefNames = _infPatRefNames
  }

  def getInfPatCompositeName(slotIdx:Int):String = {
    infPatRefNames(slotIdx)
  }

  def getInfPatCompositeInSlot(slotIdx:Int):PatternMatchInfPat = {
    infPatSlots(slotIdx)
  }

  def getInfPatCompositeSlotIdx(name:String):Int = {
    for (i <- 0 until infPatSlots.length) {
      if (infPatRefNames(i) == name) return i
    }
    // Default return
    -1
  }

  // Check that the row reference requirements for this composite pattern have been met (i.e. that the row UUIDs for those specified as having to match, do infact match)
  // Must be run after compositeInheritVariables(), so that row UUIDs from infPatSlots are populated.
  // Note: assumes that all variableValueLUTs have the same row references populated -- it would be unusual for this not to be the case.
  // Returns true if row reference requirements are met.
  def checkCompositeRowRefReq(tablestore:TableStore): Boolean = {
    val rowEquivs = inferencePattern.compInfReq.get.rowEquiv

    //## println (" * checkCompositeRowRefReq: started... ")
    val lut = variableValueLUT
    for (rowEquiv <- rowEquivs) {
      //## println ("rowEquiv.ident1: " + rowEquiv.ident1 + "  UUID: " + lut.getVariable(rowEquiv.ident1).mkString(",") + "    " + tablestore.getRowByUID(lut.getVariableStr(rowEquiv.ident1).replaceAll("UUID", "")).toStringText() )
      //## println ("rowEquiv.ident2: " + rowEquiv.ident2 + "  UUID: " + lut.getVariable(rowEquiv.ident2).mkString(",") + "    " + tablestore.getRowByUID(lut.getVariableStr(rowEquiv.ident2).replaceAll("UUID", "")).toStringText() )
      //## if (lut.getVariable(rowEquiv.ident1).deep != lut.getVariable(rowEquiv.ident2).deep) {
      if (!lut.getVariable(rowEquiv.ident1).compare( lut.getVariable(rowEquiv.ident2) )) {
        //## println (" NOT EQUAL")
        return false
      } else {
        //## println (" EQUAL")
      }
    }
    // If we reach here, the row reference requirements are valid for this VariableLUT

    //## println (" Returning TRUE")
    // Return
    true
  }

  // Grab the variable LUT(s) from the infPatSlots, and bring them into this variableLUT, with new, longer variable names based on their inference pattern slot name (e.g. y.<SOM1> -> x.y.<SOM1>)
  // TODO: NOTE: This function currently modifies the VariableLUTs in 'variableValueLUT', but no longer guarantees that each is valid -- invalid combinations must be removed.
  def compositeInheritVariables(lexicon:Lexicon[String]) {
    // Step 1: Case: If there are no variable value LUTs (as in the case where a composite pattern may have no rows of it's own), then add a blank variable value LUT.
    /*
    // depricated: No longer required with single variable LUT
    if (variableValueLUT.size == 0) {
      if (infPatSlots.length > 0) {
        variableValueLUT.append( new VariableValueLUT(lexicon) )
      }
    }
    */

    // Step 2: Add variable LUTs with inheritance prefix
    for (slotIdx <- 0 until infPatSlots.length) {
      val prefix = infPatRefNames(slotIdx) + "."
      //TODO: It's now assumed that there will be no alternates in the table rows for various implementation reasons, so this is removed.
      //## println (" WARNING: compositeInheritVariables() is only partially implemented, and using variableValueLUT(0) instead of all variableValueLUT()s. Data will likely be lost.")

      //TODO: Enumerate over all variableLUTs in variableValueLUT list -- but how to handle this? (e.g. create new patternmatch clones for each variableValueLUT? or another alternative?)
      variableValueLUT.addWithPrefix(prefix, infPatSlots(slotIdx).variableValueLUT)
    }


  }

  // Get the names of all instances required by all inference patterns in a composite inference pattern
  def getInstanceNamesComposite():Set[String] = {
    val numInfPatSlots = infPatSlots.length
    val out = mutable.Set[String]()

    for (i <- 0 until numInfPatSlots) {
      val name = infPatRefNames(i)
      val infPatInstanceReq = infPatSlots(i).inferencePattern.instanceRequirements.instances
      for (instReq <- infPatInstanceReq) {
        // Store instance names as 'infPatRef.instanceName', e.g. 'x.substance1'
        out += ( name + "." + instReq.name )
      }
    }

    // Return
    out.toSet
  }

  // Generate an instance remapping for the inference patterns within a composite inference pattern
  def getInstanceRemapComposite(infSlotIdx:Int, originalRemap:Remap):(Remap, Boolean) = {
    // Get (global) mapping:
    val (instMapping, success) = inferencePattern.getInstanceMapComposite()
    println ("getInstanceMapComposite: " + instMapping.toString)

    // Get reference name of slot in question
    val slotName = infPatRefNames(infSlotIdx)
    val instNames = infPatSlots(infSlotIdx).inferencePattern.getInstanceNames()

    val remapInst = new RemapUni
    for (name <- instNames) {
      val replaceWithInternal = instMapping(slotName + "." + name)
      val externalName = originalRemap.instances.getRemap(replaceWithInternal)
      if (externalName.isDefined) {
        val replaceWithExternal = externalName.get
        remapInst.addRemap(find = name, replace = replaceWithExternal)
      } else {
        println ("ERROR: Could not find remapping for '" + replaceWithInternal + "'")
        return (new Remap(instances = new RemapUni), false)
      }
    }

    //##
    println (" * getInstanceRemapComposite: " + remapInst.remap.toString())

    // Return success
    (new Remap( instances = remapInst ), true)
  }

  // As above, but generates an array with all remaps
  def getAllInstanceRemapsComposite(originalRemap:Remap):(Array[Remap], Boolean) = {
    val out = new Array[Remap](infPatSlots.length)

    for (i <- 0 until infPatSlots.length) {
      val (result, success) = getInstanceRemapComposite(i, originalRemap)
      if (!success) {
        return (Array.empty[Remap], false)
      }
      out(i) = result
    }

    // Return success
    (out, true)
  }

  /*
   * Instance Match population/use
   */

  // Check to see if this pattern has any unprocessed instance matches
  //## Modified to work with constraint framework
  def hasUnprocessedConstraints:Boolean = {
    println ("hasUnprocessedConstraints:")
    for (constraintEval <- constraintEvals) {
      if (constraintEval.hasUnprocessedConstraints()) return true
    }

    // Default return
    false
  }

  // Return whether the constraint (including instance) for this pattern can be met
  def canContraintsBeMet():Boolean = {
    for (constraint <- constraintEvals) {
      if (constraint.satisfiesConstraints()) return true
    }
    // Default return
    return false
  }

  /*
  // NOTE: I think this may be functionally a duplicate of checkIfValidRemap()?

  def canConstraintsBeMetInstances(remapping:Remap):Boolean = {
    for (constraint <- constraintEvals) {
      if (constraint.satisfiesConstraints()) {
        val constraintRemap = constraint.getRemap()
        if (remapping.hasSubset(constraintRemap)) return true
      }
    }

    // Default return
    false
  }

   */


  // Convert one set of constraints into an instance remap prescription
  def getInstanceRemap(constraintIdx:Int):RemapUni = {
    val remap = new RemapUni
    val instanceMaps = constraintEvals(constraintIdx).instanceMaps

    for (pair <- instanceMaps) {
      remap.addRemap( pair.patternInstanceName, pair.existingInstanceName )
    }

    // Return
    remap
  }


  // Check if a given remap has a valid entry in this PatternMatchInfPat.
  // If so, return success and constraint evaluation index.
  // Return is (success, constraintIdx)
  // TODO: Does not work with composite patterns? (needs to take nested remapping into account)
  def checkIfValidRemap(in:RemapUni):(Boolean, Int) = {
    for (constraintIdx <- 0 until constraintEvals.length) {
      val constraintEval = constraintEvals(constraintIdx)
      val instanceMaps = constraintEvals(constraintIdx).instanceMaps

      //## println ("Checking constraintIdx: " + constraintIdx)
      //## println (constraintEval.toString())

      breakable {
        for (i <- 0 until instanceMaps.length) {
          //## println ("Checking instanceMap: " + i)
          val pair = instanceMaps(i)
          //## println ("Comparing InstanceMap:" + pair.toString() + " (existingInstanceName: " + pair.existingInstanceName + ")")

          val replace = in.getRemap(find = pair.patternInstanceName)
          if (replace.isDefined) {
            //## println ("  with: " + replace.get.toString())
            if (replace.get != pair.existingInstanceName) {
              //## println ("break1")
              break()
            }
          } else {
            //## println ("break2")
            break()

          }
        }

        //## println ("made it")
        // If we reach here, the remapping is the same as this index.  Check for valid constraint satisfaction.
        val constraintsSatisfied = constraintEval.satisfiesConstraints()
        // Return
        return (constraintsSatisfied, constraintIdx)
      }
    }

    // If we reach here, the instance mappings were not found in any constraint evaluation -- return a -1 index.
    // Return
    //## println ("no match found")
    (false, -1)
  }


  /*
   * Variable LUT
   */

  def getVariableLUT():VariableValueLUT = {
    variableValueLUT
  }

  /*
   * Checking variables match
   */

  // Add variables externally
  def addVariables(in:VariableValueLUT): Unit = {
    variableValueLUT.add(in)
  }

  // TODO: Likely need to rewrite this to work with the new FlexiblePatternMatchCell
  def checkVariablesMatch():Boolean = {
    println ("WARNING: checkVariablesMatch(): UNIMPLEMENTED")
    return true
    /*
    val variables = mutable.Set[String]()
    val numUsages = mutable.Map[String, Int]().withDefaultValue(0)

    // Hack to get a lexicon reference (TODO: Clean this up)
    if (rowSlots.length < 1) return false
    val lexicon = rowSlots(0).row.lexicon

    // Clear any existing series in the variable value look-up table
    variableValueLUT.clear()

    //## Profiler.start("Step1-getvarlist")
    // Step 1: Get a list of variables from each of the patterns
    for (i <- 0 until rowSlots.length) {
      val cellPatMatches = rowSlots(i).getMatches()
      for (cellPat <- cellPatMatches) {
        val patternVariables = cellPat.cellPat.getAllVariables
        variables ++= patternVariables       // Get all variables from this pattern.  Note: filter opt stars off names, since here we're looking for a count.
        //## println ("v: " + patternVariables)
        for (variable <- patternVariables) {
          numUsages(variable) += 1
        }
      }
    }
    //## Profiler.end("Step1-getvarlist")

    //TODO: Check all alternatives

    // Create iterator to iterate through all (rowNum, cellNum, alternativeNum) alternatives.
    // For each slot
    //## Profiler.start("Step2-createiter")
    val rowIndices = new ArrayBuffer[ (Int, Int) ]
    val maxValues = new ArrayBuffer[Int]
    var idx:Int = 0
    for (slotNum <- 0 until numSlots) {
      val cellPats = rowSlots(slotNum).getMatches()
      // For each cell
      for (cellNum <- 0 until cellPats.length) {
        val cellPat = cellPats(cellNum)

        rowIndices.append( (slotNum, cellNum) )

        maxValues.append(cellPat.getNumAlternatives)
      }
    }
    //## Profiler.end("Step2-createiter")

    //## println ("rowIndices:" + rowIndices.toList)
    //## println ("maxValues: " + maxValues.toList)

    // Create iterator
    //## Profiler.start("Step2a-createiter")
    val iter = new CombinationIterator(maxValues.toArray)
    //## Profiler.end("Step2a-createiter")


    //## println ("NumUsages: " + numUsages)
    //## println ("Variable names: " + variables.mkString(", "))

    //## Profiler.start("Step3-outer")
    while (iter.hasNext()) {
      //## Profiler.start("Step3-iternext")
      val altIndices = iter.next()
      //## Profiler.end("Step3-iternext")

      breakable {

        //## Profiler.start("Step3-1")
        // Look-up table for variables and values.  Only populated after calling checkVariablesMatch(), and if that call is successful.
        val variableValueLUTLocal = new VariableValueLUT(lexicon)

        // Step 2A: Check to make sure that the values of the variables are consistent across the patterns
        for (variableName <- variables) {
          //## println("variableName: " + variableName)

          // Check to see whether the variable exists in more than one context.  If it doesn't, we don't need to check that all the values are the same.
          if (numUsages(variableName) > 1) {
            var isOptional = isVariableOptional(variableName)
            // Retrieve all values that this variable takes with this combination of rows, cells, and alternatives.
            val possibleValues = collectVariableValues(variableName, rowIndices.toArray, altIndices)
            val (areIdentical, singleValue) = areVariableValuesIdentical(possibleValues, isOptional)

            if (!areIdentical) {
              // This variable had multiple values, or was not found
              //## println ("Invalid Combination!")
              break()
            } else {
              // If the variable is non-optional, check that it is not blank.  If it is optional, it's okay if it's blank.
              if (((!isOptional) && (singleValue.length > 0)) || isOptional) {
                // This vairable had a valid, single value -- store
                variableValueLUTLocal.setVariable(variableName, singleValue)
              } else {
                break()
              }
            }
          }
        }
        //## Profiler.end("Step3-1")

        // If we reach this point without breaking, then the combination is valid (with the caveat that, for patterns with optional variables, we still need to verify the
        // edge case where the same variable is both optional and non-optional in the same pattern.
        //## println ("Valid Combination!")
        //## println ( variableValueLUTLocal.toString() )

        //## Profiler.start("Step3-2")
        // TODO: Go back and fill in the values for the variables that only occur a single time
        for (variableName <- variables) {
          //## println("variableName: " + variableName)

          // Check to see whether the variable exists in more than one context.  If it doesn't, we don't need to check that all the values are the same.
          if (numUsages(variableName) == 1) {
            var isOptional = isVariableOptional(variableName)
            // Retrieve all values that this variable takes with this combination of rows, cells, and alternatives.
            val possibleValues = collectVariableValues(variableName, rowIndices.toArray, altIndices)
            val (areIdentical, singleValue) = areVariableValuesIdentical(possibleValues, isOptional)

            if (!areIdentical) {
              // This variable had multiple values, or was not found
              //## println ("Invalid Combination looking for a single instance of a variable -- this should never happen!")
              break()
            } else {
              // If the variable is non-optional, check that it is not blank.  If it is optional, it's okay if it's blank.
              if (((!isOptional) && (singleValue.length > 0)) || isOptional) {
                // This vairable had a valid, single value -- store
                variableValueLUTLocal.setVariable(variableName, singleValue)
              } else {
                break()
              }
            }
          }
        }
        //## Profiler.end("Step3-2")

        //##
        /*
        println ("Variable values (including single occurrence variables):")
        println ( variableValueLUTLocal.toString() )
        */

        //## Profiler.start("Step3-3")
        // Check to see whether this combination of variables and values is unique
        breakable {
          for (i <- 0 until variableValueLUT.length) {
            val existing = variableValueLUT(i)
            val namesExisting = existing.getSortedVariableNames()
            val namesLocal = variableValueLUTLocal.getSortedVariableNames()
            if (namesExisting.deep != namesLocal.deep) {
              // The two have different variables instantiated -- so they can't be the same.
            } else {
              var done:Boolean = false
              breakable {
                for (name <- namesLocal) {
                  if (existing.getVariable(name).deep != variableValueLUTLocal.getVariable(name).deep) {
                    // at least one element is different -- break
                    break()
                  }
                }
                // If we reach here, then the pattern of instantiated variables matches an existing pattern
                done = true
              }

              // If we've found a matching pattern, then break from checking other existing patterns.
              if (done) break()
            }
          }

          // If we reach here, then the pattern is unique -- store this instance of a valid variable value LUT
          //## println ("Unique -- added. ")

          // Also add row UUID values to the variable LUT
          addRowUIDsToVariableLUT( variableValueLUTLocal )
          // Store
          variableValueLUT.append( variableValueLUTLocal )
        }
        //## Profiler.end("Step3-3")
      }

    }
    //## Profiler.end("Step3-outer")

    // If we have found at least one valid set of consistent variable assignments, then return true.
    if (variableValueLUT.length > 0) {
      return true
    }

    // Default return
    false
     */
  }


  // For a given set of (rowIdx, cellIdx, altIdx, ...) indices, retreive the possible values of variable 'variableName' from this inference pattern.
  // TODO: Likely need to rewrite this to work with the new FlexiblePatternMatchCell
  /*
  def collectVariableValues(variableName:String, rowIndices:Array[(Int, Int)], cellAltIndices:Array[Int]):Array[Array[Int]] = {
    var rowIdx:Int = 0
    var cellAltIdx:Int = 0
    val values = new ArrayBuffer[Array[Int]]

    //## println("variableName: " + variableName)

    while (rowIdx < rowIndices.length) {
      val rowSlotIdx = rowIndices(rowIdx)._1
      val cellIdx = rowIndices(rowIdx)._2
      //val cellIdx = cellAltIndices(cellAltIdx)
      val altIdx = cellAltIndices(cellAltIdx)

      //## println ("rowSlotIdx: " + rowSlotIdx + "  cellIdx: " + cellIdx + "  altIdx: " + altIdx)
      // Retrieve value
      val cellPat = rowSlots(rowSlotIdx).getMatches()(cellIdx)
      val localValue = cellPat.getVariableValue(variableName, altIdx)

      // If value exists
      if (localValue.isDefined) {
        // Check that it's not a duplicate
        breakable {
          for (i <- 0 until values.length) {
            if (values(i).deep == localValue.get) break() // Found duplicate
          }
          // No duplicate found -- store
          values.append(localValue.get)
        }
      }

      // Increment index
      rowIdx += 1
      cellAltIdx += 1
    }

    // Return
    values.toArray
  }
   */

  // Checks to see if all the inner Array[Int]s in the outter Array[Array[Int]] are identical.  If they are, then it returns the unique Array[Int].
  // If not, then it returns Array.empty[Int].
  // IsOptional signifies whether this variable is optional or not -- if so, blank cases will be ignored.
  // Boolean return signifies whether the items in the array are unique or not.
  def areVariableValuesIdentical(in:Array[Array[Int]], isOptional:Boolean = false):(Boolean, Array[Int]) = {
    // Bound checking
    if (in.length < 1) return (false, Array.empty[Int])
    if (in.length == 1) return (true, in(0))

    /*
    // Depricated -- this code allowed specific cases of variables that were optional to both have a value and no value at the same time.
    // For variables that are optional
    if (isOptional) {
      // Remove blank elements
      in = removeEmptyElems(in)
      // If no elements remain, return blank variable
      if (in.length < 1) return (true, Array.empty[Int])
    }
    */

    for (i <- 1 until in.length) {
      //if ( in(i).deep != in(0).deep ) {
      if (!java.util.Arrays.equals(in(i), in(0))) {
        return (false, Array.empty[Int])
      }
    }

    // Return
    (true, in(0))
  }


  def removeEmptyElems(in:Array[Array[Int]]):Array[Array[Int]] = {
    val out = new ArrayBuffer[Array[Int]]
    for (elem <- in) {
      if (elem.length > 0) out.append(elem)
    }
    // Return
    out.toArray
  }

  // This function makes a variable name for each row name, and populates it with the UUID for that row.
  // This provides a mechanism for specifying rows have the same UUID in composite patterns (e.g. musthave (x.row1 == y.row2))
  def addRowUIDsToVariableLUT() {
    val patMatchHash = new StringBuilder    // Also store a hash pattern for this inference pattern match

    for (slotIdx <- 0 until numSlots) {
      val rowName = getRowName(slotIdx)

      // Retrieve row UUID
      var uuid = ""
      if (rowSlots(slotIdx).isDefined) {
        uuid = rowSlots(slotIdx).get.row.uid
      }
      if (uuid == TableRow.DEFAULT_ROW_UUID) {
        // Handle case where UUID was not included in tablestore, and was replaced with a default human-readable string (e.g. "No UUID specified").
        // In these cases, set a blank UUID, so these can be detected with e.g. 'if (x.row1 == "") then '
        uuid = ""
      }

      // Add to variable look-up-table
      if (variableValueLUT.exists(rowName)) {
        println ("ERROR: addRowUIDsToVariableLUT: A variable name and row name are identical ('" + rowName + "') in inference pattern (" + inferencePattern.name + "). Row UUID value not be set. ")
      } else {
        var uuidStr = ""
        if (uuid.length > 0) uuidStr = UUID_PREFIX + uuid         // If the UUID is blank (e.g., from a row being optional), don't populate the UUID_PREFIX so we can do simple matching to figure out if the row is populated or not (e.g. if (rowname != "") ... )
        variableValueLUT.setVariable(rowName, uuidStr)
        patMatchHash.append(uuidStr)
      }

    }

    // Store inference pattern match hash
    variableValueLUT.setVariable(inferencePattern.name, PATMATCH_PREFIX + patMatchHash.toString())

  }

  /*
   * Supporting functions for optional variables (marked with a *, e.g. "<*SOM>")
   */
  def isVariableOptional(name:String):Boolean = {
    if (name.startsWith( InferencePattern.VARIABLE_OPT_MARKER ) && (name.length > 1)) return true
    // Default return
    false
  }

  def removeOptMarker(name:String):String = {
    name.substring( InferencePattern.VARIABLE_OPT_MARKER.length )
  }

  // Optional variables are signified by a leading "*".  This function removes leading "*"'s from a set of variable names.
  def removeMarkerFromOptVariables(in:Set[String]):Set[String] = {
    val out = mutable.Set[String]()
    for (variableName <- in) {
      if (isVariableOptional(variableName)) {
        out += removeOptMarker( variableName )
      } else {
        out += variableName
      }
    }

    // Return
    out.toSet
  }


  /*
   * Hashcode for row population
   */
  def getHashcode():String = {
    var hash:Int = 0

    // Hash for this pattern match
    for (rowSlot <- rowSlots) {
      // Add the individual row hashes
      if (rowSlot.isDefined) {
        hash = (hash + rowSlot.get.getRowHashCode()) % 1000000
      }
    }

    // If a composite pattern, also add the hashes for each of the composite patterns
    for (compositePat <- infPatSlots) {
      hash = (hash + compositePat.getHashcodeInt()) % 1000000
    }

    // Ensure hashcodes are positive integers
    if (hash < 0) hash = hash * -1

    // Return
    hash.toLong.toHexString
  }


  def getHashcodeInt():Int = {
    var hash: Int = 0

    // Hash for this pattern match
    for (rowSlot <- rowSlots) {
      // Add the individual row hashes
      if (rowSlot.isDefined) {
        hash = (hash + rowSlot.get.getRowHashCode()) % 1000000
      }
    }

    // Ensure hashcodes are positive integers
    if (hash < 0) hash = hash * -1

    // Return
    hash
  }


  /*
   * Accessors for downstream tasks
   */
  def getPatternUUIDs():(Set[String], Set[TableRow]) = {
    val outUUID = mutable.Set[String]()
    val outTableRow = mutable.Set[TableRow]()

    val rowsToExport = collectRows(traverseCompositePattern = true)

    // Step 1A: Export rows
    for (rowToExport <- rowsToExport) {
      val patternMatch = rowToExport._2
      if (patternMatch.isDefined) {
        val uuid = patternMatch.get.row.uid
        if (uuid.length > 0) {
          outUUID.add(uuid)
          outTableRow.add(patternMatch.get.row)
        }
      }
    }

    // Return
    (outUUID.toSet, outTableRow.toSet)
  }


  /*
   * JSON Export
   */

  def toJSON():String = {
    val os = new StringBuilder

    // Step 0: Open object
    os.append("\t\t{\n")

    // Step 1: Collect rows
    val rowsToExport = collectRows(traverseCompositePattern = true)

    // Step 1A: Export rows (short summary)
    val matchUUIDs = new ArrayBuffer[String]()
    for (rowToExport <- rowsToExport) {
      val patternMatch = rowToExport._2
      if (patternMatch.isDefined) {
        val uuid = patternMatch.get.row.uid
        if (uuid.length > 0) {
          matchUUIDs.append(uuid)
        }
      }
    }
    os.append("\t\t\"uuids\": [")
    for (i <- 0 until matchUUIDs.length) {
      os.append("\"" + matchUUIDs(i) + "\"")
      if (i < (matchUUIDs.length-1)) os.append(",")
    }
    os.append("],\n")


    // Step 1B: Export rows (detailed)
    os.append("\t\t\"slots\": [\n")
    for (i <- 0 until rowsToExport.length) {
      // Create JSON string
      val name = rowsToExport(i)._1
      val patternMatch = rowsToExport(i)._2
      val pattern = rowsToExport(i)._3

      val json = mkJSONString(name, patternMatch, pattern)
      os.append("\t\t\t" + json)
      if (i < (rowsToExport.length - 1)) os.append(",")
      os.append("\n")
    }
    os.append("\t\t], \n")


    // Step 2: Collect variables
    os.append("\t\t\"variables\": [\n")
    val variableNames = variableValueLUT.getSortedVariableNames()
    val varStrs = new ArrayBuffer[String]()
    for (i <- 0 until variableNames.length) {
      val variableName = variableNames(i)
      val variableValue = getVariableValueString(variableName, lexicon)

      if (!variableValue.startsWith(UUID_PREFIX) && !variableValue.startsWith(PATMATCH_PREFIX)) {
        // Do not include row UUIDs or inference pattern match hashes
        varStrs.append("\t\t\t{ \"name\":\"" + variableName + "\", \"value\":\"" + getVariableValueString(variableName, lexicon) + "\" }")
      }
    }
    os.append( varStrs.mkString(",\n") + "\n" )
    os.append("\t\t], \n")

    // Step 2A: Report variables as their original word (instead of lemma) forms
    os.append("\t\t\"variablesOrigTextInferred\": [\n")
    val varsInferred = collectVariablesAsOriginalText()
    val varStrsInferred = new ArrayBuffer[String]()
    for (varName <- varsInferred.keySet.toArray.sorted) {
      varStrsInferred.append("\t\t\t{ \"name\":\"" + varName + "\", \"value\":\"" + varsInferred(varName) + "\" }")
    }
    os.append( varStrsInferred.mkString(",\n") + "\n" )
    os.append("\t\t]\n")

    // Step 3: Close object
    os.append("\t\t}")

    // Return
    os.toString
  }

  // This attempts to determine what the original (non-lemmatized) version of the variables might have been.
  private def collectVariablesAsOriginalText(): Map[String, String] = {
    val out = new mutable.HashMap[String, String]()   // (varName, value)

    val variableNames = variableValueLUT.getSortedVariableNames()
    for (variableName <- variableNames) {
      val variableValue = getVariableValueString(variableName, lexicon)
      if (!variableValue.startsWith(UUID_PREFIX) && !variableValue.startsWith(PATMATCH_PREFIX)) {
        val variableValueLexiconIDs = variableValueLUT.getVariable(variableName)

        val newValue = new ArrayBuffer[Array[Int]]
        for (wordIdx <- 0 until variableValueLexiconIDs.value.length) {
          val lexIdxsThisWord = variableValueLexiconIDs.value(wordIdx)

          // Just take the first word, right now
          val substitutons = getWordsFromLemmaIdx(lexIdxsThisWord(0))
          if (substitutons.length > 0) {
            println ("###: Found substitutions for " + lexicon.get( lexIdxsThisWord(0) ) + " (" + LexiconUtil.lexiconIdxsToStr(substitutons, lexicon) + ")")
            newValue.append(substitutons)
          } else {
            println ("ERROR: Unable to find substitution for " + lexicon.get( lexIdxsThisWord(0) ))
            newValue.append( Array(lexIdxsThisWord(0)) )
          }
        }

        // Pick one alternative (Just make a flat array out of the first element in each array)
        val oneAlternative = new ArrayBuffer[Int]
        for (arr <- newValue) {
          oneAlternative.append(arr(0))
        }

        // Convert it into a string
        val valueStr = LexiconUtil.lexiconIdxsToStr(oneAlternative.toArray, lexicon)
        out(variableName) = valueStr
      }
    }

    // Return
    out.toMap
  }

  // Helper for above
  private def getWordsFromLemmaIdx(lemmaIdx:Int):Array[Int] = {
    val out = new ArrayBuffer[Int]

    for (row <- rowSlots) {
      if (row.isDefined) {
        for (flexMatch <- row.get.cellMatches) {
          for (oneMatch <- flexMatch.matches) {
            val lemmas = oneMatch.spanLemmas
            val words = oneMatch.spanWords
            for (i <- 0 until lemmas.length) {
              val lemma = lemmas(i)
              val word = words(i)
              if (lemma == lemmaIdx) {
                out.append(word)
              }
            }
          }
        }
      }
    }

    // Return
    out.toArray
  }


  private def mkJSONString(rowSlotName:String, patternMatch:Option[PatternMatchRow], pattern:TableRowPattern):String = {
    val os = new StringBuilder

    // Start object
    os.append("{")

    // Fill elements
    os.append("\"rowSlotName\": \"" + rowSlotName + "\", ")

    var uuid:String = ""
    if (patternMatch.isDefined) uuid = patternMatch.get.row.uid
    os.append("\"uuid\": \"" + uuid + "\", ")

    var rowText:String = ""
    if (patternMatch.isDefined) rowText = patternMatch.get.row.toStringText()
    os.append("\"rowText\": \"" + rowText + "\", ")

    val rowStatic:Boolean = pattern.hasStaticUUIDs
    os.append("\"rowStatic\": \"" + rowStatic + "\", ")

    val rowOptional:Boolean = pattern.isOptional
    os.append("\"rowOptional\": \"" + rowOptional + "\", ")

    var relaxedCount:Int = 0
    if (patternMatch.isDefined) relaxedCount = patternMatch.get.getRelaxedCount()
    os.append("\"relaxedCount\": \"" + relaxedCount + "\"")

    // End object
    os.append("}")


    // Return
    os.toString
  }

  // Function to recursively collect row matches (specifically, the slot name and it's (optional) match), from a normal or composite pattern.
  private def collectRows(traverseCompositePattern:Boolean = false, compositePrefix:String = ""):Array[(String, Option[PatternMatchRow], TableRowPattern)] = {
    val out = new ArrayBuffer[(String, Option[PatternMatchRow], TableRowPattern)]

    // This pattern
    for (i <- 0 until numSlots) {
      val name = compositePrefix + rowNames(i)
      out.append ( (name, rowSlots(i), inferencePattern.rowPatterns(i)) )
    }

    if ((traverseCompositePattern) && (isComposite)) {
      for (infPatIdx <- 0 until infPatSlots.length) {
        val infPatName = infPatRefNames(infPatIdx)
        for (infPatSlot <- infPatSlots) {
          out.insertAll(out.size, infPatSlot.collectRows(traverseCompositePattern = true, compositePrefix = compositePrefix + infPatName + "."))
        }
      }
    }

    // Return
    out.toArray
  }


  /*
   * String methods
   */
  override def toString():String = {
    val os = new StringBuilder

    for (i <- 0 until rowSlots.length) {
      os.append("rowSlot " + i + ": \n")
      if (isPopulated(i)) {
        os.append( rowSlots(i).toString() )
      } else {
        os.append ("Empty")
      }
      os.append("\n")
    }

    os.append("Variables and assignments: " + variableValueLUT)

    // Return
    os.toString
  }


  def toString(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (i <- 0 until rowSlots.length) {
      os.append("rowSlot " + i + " (" + rowNames(i) + "): \n")
      if (isPopulated(i)) {
        if (rowSlots(i).isDefined) {
          os.append(rowSlots(i).get.toString(lexicon))
        }
      } else {
        os.append ("Empty")
      }
      os.append("\n")
    }

    os.append("Variables and assignments: \n")
    os.append(variableValueLUT.toString() + "\n")

    /*
    val keys = variableValueLUT.keySet.toArray.sorted
    for (i <- 0 until keys.length) {
      val key = keys(i)
      os.append(key + "->")
      for (lexIdx <- variableValueLUT(key)) {
        os.append(" " + lexicon.get(lexIdx))
      }

      if (i < keys.length-1) os.append(", ")
    }
    */

    // Return
    os.toString
  }


  def toStringMinimalSummary():String = {
    val os = new StringBuilder

    os.append("Inference Pattern: " + inferencePattern.name + "\n")

    for (i <- 0 until rowSlots.length) {
      os.append("rowSlot " + i + ": ")
      if (isPopulated(i)) {
        if (rowSlots(i).isDefined) {
          os.append(rowSlots(i).get.toStringMinimal())
        }
      } else {
        os.append ("Empty")
      }
      os.append("\n")
    }

    os.append("Variables and assignments: " + variableValueLUT)

    // Return
    os.toString
  }


  def toHTMLString(lexicon:Lexicon[String], constraintHighlight:Int = -1):String = {
    val os = new StringBuilder


    //##os.append( verboseRowMatchSummaryToHtml(lexicon) )
    //os.append( rowSlotSummaryToHtml(lexicon) )
    //os.append( compositeSummaryToHtml(lexicon) )
    os.append( rowSlotAndCompositeSummaryToHtml(lexicon) )

    os.append( variableListToHtml(lexicon) )
    //## os.append( instanceMatchesToHtml() )
    os.append( constraintMatchesToHtml(constraintHighlight, detailed = true) )

    // Return
    os.toString
  }



  // Verbose preformatted summary
  def verboseRowMatchSummaryToHtml(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append("<pre>\n")
    for (i <- 0 until rowSlots.length) {
      os.append("rowSlot " + i + " (" + rowNames(i) + "): \n")
      if (isPopulated(i)) {
        if (rowSlots(i).isDefined) {
          os.append(rowSlots(i).get.toString(lexicon))
        }
      } else {
        os.append ("Empty")
      }
      os.append("\n")
    }
    os.append("</pre>\n")

    // Return
    os.toString()
  }

  def rowSlotAndCompositeSummaryToHtml(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append( rowSlotSummaryToHtml(lexicon) )
    os.append( compositeSummaryToHtml(lexicon) )

    // Return
    os.toString()
  }

  // Row slots, with specific table rows that they're populated with
  def rowSlotSummaryToHtml(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    os.append("Summary:<br>\n")
    os.append("<table class=\"log\" id=\"t01\">\n")
    os.append("<tr class=\"log\">")
    os.append("<th class=\"log\">#</th>")
    os.append("<th class=\"log\">Sta</th>")
    os.append("<th class=\"log\">Rlx</th>")
    os.append("<th class=\"log\">Name</th>")
    os.append("<th class=\"log\">Row Text</th>")
    os.append("</tr>\n")

    for (i <- 0 until rowSlots.length) {
      os.append("<tr class=\"log\">")
      os.append("<td class=\"log\">" + i + "</td>")

      os.append("<td class=\"log\">")
      if (rowSlots(i).isDefined && rowSlots(i).get.generatedFromStaticUUID) {
        os.append("ST")
      }
      os.append("</td>")

      os.append("<td class=\"log\">")
      if (rowSlots(i).isDefined) os.append(rowSlots(i).get.getRelaxedCount())
      os.append("</td>")

      /*
      println ("###")
      println (rowSlots(i).cellMatches.length)
      println (rowSlots(i).toString(lexicon))
      */

      os.append("<td class=\"log\">" + rowNames(i) + "</td>")
      if (isPopulated(i)) {
        os.append("<td class=\"log\">")
        if (rowSlots(i).isDefined) os.append(rowSlots(i).get.toStringVariableHighlights(lexicon))
        os.append("</td>")
      } else {
        os.append ("<td class=\"log\"> Empty </td>")
      }
      os.append("</tr>\n")
    }
    os.append("</table>")
    os.append("<br>")

    os.append("Relaxed (dynamic): " + getRelaxedCountDynamicRowsOnly() + " &nbsp;&nbsp;&nbsp;&nbsp; ")
    os.append("Relaxed (total): " + getRelaxedCountTotal() + "<br>")
    os.append("Optional slots unpopulated: " + getNumOptionalUnpopulated() + "<br>")

    // Return
    os.toString()
  }

  def compositeSummaryToHtml(lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    // Composite patterns
    if (isComposite) {
      os.append("Composite Patterns: <br>\n")
      var numInfPatSlots = infPatSlots.length
      for (i <- 0 until numInfPatSlots) {
        os.append("\t<b>InfPatSlot " + i + " (" + infPatRefNames(i) + ":" + infPatSlots(i).inferencePattern.name + ")</b><br>\n")
        os.append("<blockquote>")

        if (infPatSlots(i).isComposite) {
          // Another composite pattern -- recurse
          //os.append( infPatSlots(i).compositeSummaryToHtml(lexicon))
          os.append( infPatSlots(i).rowSlotAndCompositeSummaryToHtml(lexicon) )
        } else {
          // A non-composite (regular, and terminal) inference pattern.
          os.append(infPatSlots(i).rowSlotSummaryToHtml(lexicon))
        }
        os.append("</blockquote>")
      }

    }

    // Return
    os.toString()
  }

  // Variables and assignments
  def variableListToHtml(lexicon:Lexicon[String], removeUUIDs:Boolean = true):String = {
    val os = new StringBuilder

    os.append("Variables and assignments: <br>\n")
    val variableNames = variableValueLUT.getSortedVariableNames()
    os.append("<table class=\"log\" id=\"t01\">\n")
    os.append("<tr class=\"log\">")
    os.append("<th class=\"log\">Name</th>")


    os.append("<th class=\"log\">Values</th>")
    os.append("</tr>\n")

    for (i <- 0 until variableNames.length) {
      val variableName = variableNames(i)
      val variableValue = getVariableValueString(variableName, lexicon)

      if ((!variableValue.startsWith(UUID_PREFIX) && !variableValue.startsWith(PATMATCH_PREFIX)) || (!removeUUIDs)) {
        // Do not include row UUIDs or inference pattern match hashes
        os.append("<tr class=\"log\">")

        os.append("<td class=\"log\">" + variableName + "</td>")
        os.append("<td class=\"log\">")
        os.append( getVariableValueString(variableName, lexicon) )
        os.append("</td>")

        os.append("</tr>\n")
      }
    }
    os.append("</table>\n")
    os.append("<br>")

    // Return
    os.toString()
  }


  // Constraints/Instances
  def constraintMatchesToHtml(onlySpecificIdx: Int = -1, detailed: Boolean = false): String = {
    val os = new StringBuilder


    os.append("Instance Matches based on Constraints: (" + inferencePattern.getNumInstancesRequired() + " instances required, " + constraintEvals.length + " constraint evaluation(s)) <br>\n")
    //os.append("<table id=\"t01\">\n")
    os.append("<table class=\"log\">\n")

    // Header
    os.append("<tr class=\"log\" style=\"background-color: #cce4ff\">")
    os.append("<th class=\"log\">Combination</th>")
    os.append("<th class=\"log\">VarAlt</th>")
    os.append("<th class=\"log\">Instance (Pattern) </th>")
    os.append("<th class=\"log\">Instance (State) </th>")
    os.append("<th class=\"log\">Meets Requirements? </th>")
    os.append("<th class=\"log\">KindOf </th>")
    os.append("<th class=\"log\">Eval(KindOf) </th>")
    os.append("<th class=\"log\">Eval(MustHave) </th>")
    os.append("<th class=\"log\">Eval(ShouldHave) </th>")
    os.append("</tr>\n")

    var matchNum: Int = 1

    for (constIdx <- 0 until constraintEvals.length) {
      // Determine whether to display all constraint evaluations, or only constraint evaluations for a specific constraint indices.
      if ((onlySpecificIdx == -1) || (onlySpecificIdx == constIdx)) {
        val overallConstraintEval = constraintEvals(constIdx)

        // Check that this set of constraints can be satisfied
        //if (overallConstraintEval.satisfiesConstraints()) {
        // Check that this pattern has instance requirements
        if (overallConstraintEval.hasInstanceRequirements()) {
          val satisfiesConstraints = overallConstraintEval.satisfiesConstraints()

          // Change the text colour depending on whether the constraints can be met (or not)
          var fontColStrStart = "<font color=\"grey\">"
          var fontColStrEnd = "</font>"
          if (satisfiesConstraints) {
            fontColStrStart = ""
            fontColStrEnd = ""
          }


          for (instMapEval <- overallConstraintEval.instanceMaps) {
            if (matchNum % 2 == 1) {
              os.append("<tr  class=\"log\"style=\"background-color: #eeeeee\">")
            } else {
              os.append("<tr  class=\"log\"style=\"background-color: #ffffff\">")
            }
            os.append("<td class=\"log\">" + fontColStrStart + constIdx + fontColStrEnd + "</td>") // Combination

            //os.append("<td>" + altNum + "</td>") // VariableLUT alternative index
            os.append("<td class=\"log\">" + fontColStrStart + 0 + fontColStrEnd + "</td>") // VariableLUT alternative index

            os.append("<td class=\"log\">" + fontColStrStart + instMapEval.patternInstanceName + fontColStrEnd + "</td>")
            os.append("<td class=\"log\">" + fontColStrStart + instMapEval.existingInstanceName + fontColStrEnd + "</td>")
            os.append("<td class=\"log\">" + fontColStrStart + satisfiesConstraints + fontColStrEnd + "</td>")

            val kindOfStr = LexiconUtil.lexiconIdxsToStr(instMapEval.existingInstance.kindof, instMapEval.existingInstance.lexicon) // TODO: Use this
            os.append("<td class=\"log\">" + fontColStrStart + kindOfStr + fontColStrEnd + "</td>")

            os.append("<td class=\"log\">" + fontColStrStart + instMapEval.isKindOfRequirementMet() + fontColStrEnd + "</td>")

            os.append("<td class=\"log\">" + fontColStrStart + overallConstraintEval.getScore(CONSTRAINT_MUSTHAVE).formatted("%3.3f") + fontColStrEnd + "</td>")
            os.append("<td class=\"log\">" + fontColStrStart + overallConstraintEval.getScore(CONSTRAINT_SHOULDHAVE).formatted("%3.3f") + fontColStrEnd + "</td>")
            os.append("</tr>\n")


            if (detailed) {
              os.append("<tr class=\"log\"><td  class=\"log\"colspan=9>" + fontColStrStart + instMapEval.existingInstance.toString().replaceAll("\n", "<br>") + fontColStrEnd + "</td></tr>")
            }
          }

          os.append("<tr class=\"log\"><td  class=\"log\" colspan=9>" + fontColStrStart + "Short Description: " + shortDescriptions(constIdx).replaceAll("\n", "<br>") + fontColStrEnd + "</td></tr>")

          if (detailed) {

            os.append("<tr class=\"log\"><td  class=\"log\" colspan=9>" + fontColStrStart + "hasUnprocessedConstraings: " + overallConstraintEval.hasUnprocessedConstraints() + "<br>" + overallConstraintEval.toString().replaceAll("\n", "<br>") + fontColStrEnd + "</td></tr>")
          }

          // Increment matchNum, which is used for alternating colour bands between rows.
          matchNum += 1

        }
        //}

      }
    }

    os.append("</table>\n")
    if (onlySpecificIdx == -1) {
      os.append("Constraints for this pattern can be met: <b>" + canContraintsBeMet() + "</b> <br>")
    }

    // Return
    os.toString()
  }


  // Get the value of one variable, at one alternative, expressed as a string
  def getVariableValueString(variableName:String, lexicon:Lexicon[String]):String = {
    variableValueLUT.getVariableStr(variableName)
  }

}



/**
  * Iterator for all combinations/permutations of a set of maxValue.size numbers, where each number has some range ( 0 to maxValue(i)-1 )
  * This version is 2 dimensional -- that is, maxValues is input and output as a 2D array of maximum values.
  * Created by peter on 7/5/18.
  */

class MultiCombinationIterator(maxValues2D:Array[Array[Int]]) {
  // Constructor

  val maxValues1D = MultiCombinationIterator.mkMultiMaxValues(maxValues2D)
  val iterator = new CombinationIterator(maxValues1D)
  val size:Long = iterator.size

  def hasNext():Boolean = {
    iterator.hasNext()
  }

  def next():Array[Array[Int]] = {
    val values1D = iterator.next()
    // Return
    MultiCombinationIterator.packValues(values1D, maxValues2D)
  }

}


object MultiCombinationIterator {

  // Convert from 2D input to 1D maxvalues
  def mkMultiMaxValues(maxValues2D:Array[Array[Int]]):Array[Int] = {
    val maxValues = new ArrayBuffer[Int]
    for (i <- 0 until maxValues2D.length) {
      for (j <- 0 until maxValues2D(i).length) {
        maxValues.append(maxValues2D(i)(j))
      }
    }
    // Return
    maxValues.toArray
  }

  // Convert from 1D output to 2D output
  def packValues(values:Array[Int], maxValues2D:Array[Array[Int]]):Array[Array[Int]] = {
    val out = new ArrayBuffer[Array[Int]]

    var idx:Int = 0
    for (i <- 0 until maxValues2D.length) {
      val outInner = new ArrayBuffer[Int]
      for (j <- 0 until maxValues2D(i).length) {
        outInner.append( values(idx) )
        idx += 1
      }
      out.append( outInner.toArray )
    }

    // Return
    out.toArray
  }



  // Example of use
  def main(args:Array[String]): Unit = {
    val maxValues2D = Array(Array(2, 2), Array(3, 4))
    val iter = new MultiCombinationIterator( maxValues2D )

    println ("size: " + iter.size)

    while (iter.hasNext()) {
      val values2D = iter.next()
      print ("(")
      for (i <- 0 until values2D.length) {
        print ("(" + values2D(i).mkString(", ") + ")")
        for (j <- 0 until values2D(i).length) {

        }
      }
      println(")")
    }

  }

}

