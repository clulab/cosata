package inferenceengineexperiments

import java.io.PrintWriter

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import explanationgraph.visualization.TableStoreHtmlExport
import explanationgraph.{CombinationIterator, TableRow, TableStore}
import inferenceengine.iml.model._
import inferenceengine.iml.parser.IMLParser
import inferenceengine.iml.runtime.{IMLReader, Interpreter}
import inferenceengine.iml.visualization.IMLHtmlExport
import inferenceengine.struct.InferencePattern
import inferenceengine.struct._
import inferenceengine.util.LemmatizerSubstitutions
import data.question.ExamQuestionUtils.{convertToExplQuestions, filterQuestionsByFlags}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import util.Histogram

/**
  * Created by peter on 3/9/2020.
  */

object ExplanationRegenationInfPatToQCProb {

  /*
   * Analyses
   */
  def analysis (questions:Array[MCExplQuestion], populatedPatterns:Array[InferencePattern], truncateToLevel:Int): String = {
    //val roleFilter = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    val roleFilter = Array("CENTRAL", "GROUNDING")

    val distsByQCCount = mutable.HashMap[String, Counter[String]]()
    val distsByQCProp = mutable.HashMap[String, Counter[String]]()
    val countsQC = new Counter[String]

    // Initialize hashmap
    for (question <- questions) {
      for (topic <- getQuestionQCLabelsTruncated(question, truncateToLevel)) {
        distsByQCCount(topic) = new Counter[String]
        distsByQCProp(topic) = new Counter[String]
      }
    }


    // For each question
    for (i <- 0 until questions.length) {
      val question = questions(i)

      println("Question " + i + ": ")
      println(question.toString())

      val (overlapCount, overlapProp) = getOverlapDistribution(question, roleFilter, populatedPatterns)
      print (overlapCount.toString())

      for (topic <- getQuestionQCLabelsTruncated(question, truncateToLevel)) {
        // Count
        val distCount = distsByQCCount(topic)
        for (key <- overlapCount.keySet) distCount.incrementCount(key, overlapCount.getCount(key))
        distsByQCCount(topic) = distCount

        // Proportion
        val distProp = distsByQCProp(topic)
        for (key <- overlapProp.keySet) distProp.incrementCount(key, overlapProp.getCount(key))
        distsByQCProp(topic) = distProp

        // QC label
        countsQC.incrementCount(topic)

      }

      println ("")
      println ("------------------------------------------------------------------------------")
      println ("")

    }




    // Average
    for (qc <- countsQC.keySet) {
      val numSamples = countsQC.getCount(qc)

      // Counts
      val distCount = distsByQCCount(qc)
      for (key <- distCount.keySet) {
        val value = distCount.getCount(key)
        var avg:Double = 0.0
        if (numSamples > 0) avg = value / numSamples

        distCount.setCount(key, avg)
      }
      distsByQCCount(qc) = distCount

      // Proportions
      val distProp = distsByQCProp(qc)
      for (key <- distProp.keySet) {
        val value = distProp.getCount(key)
        var avg:Double = 0.0
        if (numSamples > 0) avg = value / numSamples

        distProp.setCount(key, avg)
      }
      distsByQCProp(qc) = distProp
    }



    // Summary
    val osCount = new StringBuilder()
    val osProp = new StringBuilder()
    val delim:String = "\t"


    for (qc <- countsQC.keySet.toArray.sorted) {
      val numSamples = countsQC.getCount(qc)

      println ("QC Label: " + qc)
      val distCount = distsByQCCount(qc)
      println ("Distribution (count): " + distCount.toString())

      osCount.append(qc + delim)
      osCount.append(numSamples + delim)
      osCount.append("count" + delim)
      for (pair <- distCount.sorted(descending = true)) {
        val key = pair._1
        val value = pair._2
        osCount.append(key + ":" + value.formatted("%3.5f") + delim)
      }
      osCount.append("\n")


      val distProp = distsByQCProp(qc)
      println ("Distribution (proportion): " + distProp.toString())

      osProp.append(qc + delim)
      osProp.append(numSamples + delim)
      osProp.append("proportion" + delim)
      for (pair <- distProp.sorted(descending = true)) {
        val key = pair._1
        val value = pair._2
        osProp.append(key + ":" + value.formatted("%3.5f") + delim)
      }
      osProp.append("\n")


      println ("")
    }


    println ("roleFilter: " + roleFilter.mkString(", "))

    // Return
    val outStr = osCount.toString() + osProp.toString()
    outStr
  }


  // Get a list of a question's QC labels, truncated to a given truncation level
  def getQuestionQCLabelsTruncated(question:MCExplQuestion, truncateToLevel:Int):Set[String] = {
    val out = mutable.Set[String]()

    for (topic <- question.question.topic) {
      val truncatedLabel = qcLabelTruncation(topic, truncateToLevel)
      out.add(truncatedLabel)
    }

    // Return
    out.toSet
  }


  // Truncate a QC label to a specific level
  def qcLabelTruncation(label:String, truncateToLevel:Int):String = {
    val split = label.split("_")
    val slice = split.slice(0, truncateToLevel)
    // Return
    slice.mkString("_")
  }


  /*
  // Gives a complete list of possible combinations, but is memory intensive
  def mkCombinations(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[String], Set[String])], numPatternsToCombine:Int):Array[CompletionCombinationEvaluation] = {
    val out = new ArrayBuffer[CompletionCombinationEvaluation]

    val qExplUUIDs = question.getExplanationUUIDs()

    val maxValues = Array.fill[Int](numPatternsToCombine)(shortlist.size)
    val iter = new CombinationIterator(maxValues)

    var maxIntersectionSize:Int = 0

    while (iter.hasNext()) {
      val indices = iter.next()
      var completionUUIDsSum = mutable.Set[String]()

      val completionList = new Array[PatternMatchInfPat](numPatternsToCombine)
      for (i <- 0 until indices.length) {
        completionList(i) = shortlist(indices(i))._1    // PatternMatchInfPat only
        val completionUUIDs = patternMatchToUUIDSet(completionList(i))
        completionUUIDsSum = completionUUIDsSum.union(completionUUIDs)
      }

      val intersection = completionUUIDsSum.intersect(qExplUUIDs)

      // TODO: Only store if the match has a high number of intersections, to save memory?

      // Store evaluation
      // **NOTE**: For this ceiling analysis, to save time/memory, only store those overlaps that are at least as large as the maximum overlap previously encountered for this question
      // ** This means the list returned will not be complete, so not useful for downstream tasks other than ceiling performance calculation. **
      if (intersection.size >= maxIntersectionSize) {
        out.append(new CompletionCombinationEvaluation(completionList, completionUUIDsSum.toSet, intersection.toSet))
        maxIntersectionSize = intersection.size
      }
    }

    // Return
    out.toArray
  }
  */

  // For a given question, generate the distribution of overlap between the question's explanation and the best single match for each inference pattern.
  def getOverlapDistribution(question:MCExplQuestion, roleFilter:Array[String], populatedPatterns:Array[InferencePattern]):(Counter[String], Counter[String]) = {
    val outCounts = new Counter[String]()
    val outProportions = new Counter[String]()

    val explUUIDs = question.getExplanationUUIDs(roleFilter)

    // For each inference pattern, check that pattern's completions for overlap with this explanation.
    // Add any completion with non-zero overlap to a shortlist.
    val patternsWithNonZeroOverlap = new ArrayBuffer[(PatternMatchInfPat, Set[String], Set[String])]()       // Shortlist
    for (i <- 0 until populatedPatterns.length) {
      val patternName = populatedPatterns(i).name
      val completions = populatedPatterns(i).fullPatternMatches


      // Find the maximum intersection size amoung all possible matches for this pattern
      var maxIntersection:Int = 0
      for (completion <- completions) {
        // Convert from pattern match completion storage class to a set of UUIDs representing that completion
        val uuids = patternMatchToUUIDSet(completion)
        // Check for overlap between completion and question explanation
        val intersection = explUUIDs.intersect(uuids)
        // If overlap, store in shortlist

        maxIntersection = math.max(maxIntersection, intersection.size)
      }

      // Store maximum count and proportion overlap for this inference pattern
      var proportionOverlap:Double = 0.0
      if (maxIntersection > 0) {
        proportionOverlap = maxIntersection.toDouble / explUUIDs.size.toDouble
      }

      outCounts.setCount(patternName, maxIntersection)
      outProportions.setCount(patternName, proportionOverlap)
    }

    // Return
    (outCounts, outProportions)
  }

  // Convert a PatternMatchInfPat (i.e. a specific possible valid completion of a pattern) into a set of UUIDs repesenting that completion
  def patternMatchToUUIDSet(in:PatternMatchInfPat):Set[String] = {
    val out = mutable.Set[String]()
    for (slot <- in.rowSlots) {
      if (slot.isDefined) {
        out.add(slot.get.row.uid)
      }
    }
    // Return
    out.toSet
  }


  /*
   * Generate IML runscript
   */

  // Generate an IML run script that will import an entire folder of IML patterns, enumerate them using the constraint satisfaction framework, and exit.
  def generateRunScript(folder:String):String = {
    val scriptCode = new ArrayBuffer[String]

    scriptCode.append("importFolder \"" + folder + "\"")
    scriptCode.append("")
    scriptCode.append("// Perform constraint satisfaction over the inference pattern and generate the HTML logfile output. ")

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("populateInfPatMatches")    // Run patterns

    scriptCode.append("incrementState")
    scriptCode.append("exportInfPatHTML()")
    scriptCode.append("exportInfPatJSON()")
    scriptCode.append("exportTableStoreHTML()")
    scriptCode.append("exportStateSpaceHTML()")

    val scriptStr = scriptCode.mkString("\n")

    // Return
    scriptStr
  }


  /*
   * Command line parsing
   */

  def findArgumentParams(name:String, numOpts:Int, args:Array[String]):Option[Array[String]] = {
    val index = args.indexOf(name)
    if (index >= 0) {
      // Case: argument found
      return Some( args.slice(index+1, index+1+numOpts) )
    }
    // Case: argument not found
    None
  }

  /*
   * Supporting functions
   */

  // Finds the inference patterns referenced by a given script (so that they can be highlighted in output)
  def findInfPatternsUsed(statements:List[Statement]):Set[String] = {
    val out = mutable.Set[String]()

    for (statement <- statements) {
      statement match {
        case a:ExecuteInfPattern => out.add(a.patternName)
        case a:WhileLoop => out ++= findInfPatternsUsed(a.statements)
        case a:IfStatement => {
          for (conditions <- a.conditions) {
            out ++= findInfPatternsUsed(conditions.trueBranch)
          }
        }
        case _ =>
      }
    }

    // Return
    out.toSet
  }




  /*
   * Main
   */

  def main(args:Array[String]) = {
    val startTime = System.nanoTime()

    // Step 1: Parse the command line arguments, and place this in a storage class called 'props'
    val props = StringUtils.argsToProperties(args)


    // Step 2: Load the tablestore that we need to run the experiments

    // Step 2A: Find tablestore index filename from the properties list
    // The tablestore is loaded by referencing a file that includes a list of tables, called the tablestore index.
    // Load the name of the tablestore index file from the properties file.
    var tablestoreIndex: String = ""
    if (props.getProperty("tablestoreIndex", "") != "") {
      tablestoreIndex = props.getProperty("tablestoreIndex", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'tablestoreIndex' property in properties file.")
    }

    // Step 3: Load tablestore and questions
    val tablestore = new TableStore(tablestoreIndex, twoLineHeader = false, inTableSubdir = true)

    // Step 3A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)

    // Step 3B: Load questions
    val filenameQuestionsIn = props.getProperty("questionsIn", "")
    if (filenameQuestionsIn == "") throw new RuntimeException("ERROR: Unable to find 'questionsIn' property in properties file.")
    val debugQuestionLimit = StringUtils.getInt(props, "debugQuestionLimit", 0)

    // Load questions using question parser
    var questionsIn = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameQuestionsIn, fullAnnotation = false, noAnnotation = false, debugQuestionLimit = debugQuestionLimit, tsvMode = true)
    // Step 3C: Convert questions from MCQuestions to a storage class that includes the explanation for the question, MCExplQuestion
    var explQuestionsIn = convertToExplQuestions(questionsIn)

    //## Debug: show that we've loaded the first 10 questions successfully
    println("Displaying first 10 questions (debug): ")
    for (i <- 0 until 10) {
      println(explQuestionsIn(i).toString)
      println("")
    }

    // Step 3D: Filter questions to only those that have been tagged as successfully annotated
    val filteredQuestionsIn = filterQuestionsByFlags(explQuestionsIn, Array("SUCCESS", "READY"))
    println("Loaded " + filteredQuestionsIn.size + " training questions after filtering. ")



    // Step 4: Initialize parser and interpreter
    var IMLOutputPath:String = ""
    if (props.getProperty("IMLOutputPath", "") != "") {
      IMLOutputPath = props.getProperty("IMLOutputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLOutputPath' property in properties file.")
    }

    // Step 4A
    val parser = new IMLParser
    var interpreter = new Interpreter(tablestore, IMLOutputPath, outputPrefix = "")

    // depricated -- now each InferencePattern stores it's own faux interpreter
    //##InferencePattern.InitializeInterpreter(tablestore)          // Special interpreter instance in InferencePattern must be initialized


    // Step 5: Load IML files from specified folder

    // Step 5A: Retrieve the folder that the scripts reside in
    var IMLInputPath:String = ""
    if (props.getProperty("IMLInputPath", "") != "") {
      IMLInputPath = props.getProperty("IMLInputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLInputPath' property in properties file.")
    }

    // Load the maximum amount to enumerate any one inference pattern (or, set to a default value of 25000)
    val maxMatches = StringUtils.getInt(props, "maxMatches", 25000)

    // Step 5B: Automatically generate an IML runscript that will import that folder of IML patterns and enumerate them.
    val scriptStr = ExplanationRegenerationCeilingCalculator.generateRunScript(IMLInputPath, exportJSON = false, maxMatches = maxMatches)

    // Step 5C: Load and parse script
    val (program, successScript) = IMLReader.parseProgramFromString(parser, scriptStr)

    if (!successScript) {
      println ("Parse unsuccessful. ")
      sys.exit(1)
    }

    // Read the inference patterns from the primary script
    interpreter.addInferencePatterns( IMLReader.generateInferencePatterns(program, tablestore) )
    // exportInferencePatternMatching(tablestore)
    interpreter.setInferencePatternHighlight( findInfPatternsUsed(program.statements) )

    // Display the program
    println (program.toString())

    // Step 6: Run program

    // Run the main statements from the primary script
    val result = interpreter.walkOneStep(program.statements, debugDisplay = true)


    // When the interpreter stops, export the state space, tablestore, and inference pattern matching
    /*
    interpreter.populateInferencePatternMatches(interpreter)
    interpreter.exportStateSpace()
    interpreter.exportTablestoreHTML()
    interpreter.exportInferencePatternMatchingHTML()
    */

    interpreter.resetToGlobalVariableScope()

    // Step 6A: Compute the total execution time
    val endTime = System.nanoTime()
    val deltaTime = (endTime - startTime) / 1000000000L
    println ("Execution time: " + deltaTime + " seconds")

    println ("Running script complete (IMLInputPath: " + IMLInputPath + ").")
    println ("Interpreter Result: " + result)
    if (result.exit) {
      println ("Exit.")
    } else if (result.success) {
      println ("Success.")
    } else {
      println ("---------------------------------------------------------")
      println ("Error Message: ")
      println (interpreter.lastErrorStr.toString())
      println ("---------------------------------------------------------")
      println ("Statement: ")
      if (interpreter.lastStatement.isDefined) {
        println (interpreter.lastStatement)
      } else {
        println (" not defined")
      }
      println ("---------------------------------------------------------")
      println ("Failure.")
    }
    println ("Execution Complete.")


    // Step 7: Analysis
    val outputPrefix = props.getProperty("outputPrefix", "")

    val inferencePatterns = interpreter.inferencePatterns.toArray
    for (truncationLevel <- 1 to 6) {
      println (" * Level " + truncationLevel)

      val outStr = analysis(filteredQuestionsIn, inferencePatterns, truncationLevel)

      val pw = new PrintWriter("ExplRegen-InfPat2QCProb_" + outputPrefix + "_maxMatches-" + maxMatches + "_L" + truncationLevel + ".tsv")
      pw.println(outStr)
      pw.close()

    }

  }



}
