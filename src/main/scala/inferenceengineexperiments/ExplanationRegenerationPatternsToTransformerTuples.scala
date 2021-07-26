package inferenceengineexperiments


import java.io.PrintWriter

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import data.tablerankings.ExternalTableRowRankings
import edu.arizona.sista.struct.{Counter, Lexicon}
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
import scala.io.Source
import scala.util.control.Breaks._
import inferenceengine.iml.states.InferenceState
import inferenceengineexperiments.ExplanationRegenerationLowOverlap.{mkTopCombinations, patternMatchToUUIDSet}
import util.Histogram

import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by peter on 1/22/19.
  */

object ExplanationRegenerationPatternsToTransformerTuples {
  val MODE_CONNECTION_NO_CONNECTION_REQUIRED    =   0
  val MODE_CONNECTION_DATA_COLS_ONLY            =   1
  val MODE_CONNECTION_DATA_AND_FILL_COLS        =   2

  /*
   * Enumerations to tuples suitable for transformer training
   */
  def exportEnumerationTuples(inferencePatterns:Array[InferencePattern], mode:Int, filenameOut:String): Unit = {
    var totalEnumerations:Int = 0
    var tuplesOut:Long = 0

    val delim:String = "\t"
    val pw = new PrintWriter(filenameOut)

    for (infPattern <- inferencePatterns) {
      println ("Inference Pattern: " + infPattern.name)
      val tuples = enumerationsToTuples(infPattern, mode)
      val sorted = tuples.sorted(descending = true)
      for (i <- 0 until math.min(20, sorted.length)) {
        val rowTextI = sorted(i)._1._1.toStringText()
        val rowTextJ = sorted(i)._1._2.toStringText()
        println (i + "\t" + sorted(i)._2 + "\t" + rowTextI)
        println ("\t" + "\t" + rowTextJ)
      }
      println (" * Sorted list contains (" + sorted.length + ") row-row tuples from enumerations")

      println (" * Pattern (" + infPattern.name + "): " + infPattern.fullPatternMatches.length + " matches.")
      totalEnumerations += infPattern.fullPatternMatches.length

      println ("")
      println ("-------------------------------------------------------------------------------------------")
      println ("")

      // Export
      for (i <- 0 until sorted.length) {
        val patternName = infPattern.name
        val freq = sorted(i)._2
        val rowTextI = sorted(i)._1._1.toStringText()
        val rowTextJ = sorted(i)._1._2.toStringText()

        val line = new StringBuilder
        line.append(patternName + delim)
        line.append(freq + delim)
        line.append(rowTextI + delim)
        line.append(rowTextJ + delim)

        pw.println(line.toString())
        tuplesOut += 1
      }
      pw.flush()

    }


    println ("")
    println (" Total Enumerations: " + totalEnumerations)
    println (" Unique tuples (row <-> row combinations) exported: " + tuplesOut)

    pw.close()

  }


  def enumerationsToTuples(inferencePattern:InferencePattern, mode:Int):Counter[(TableRow, TableRow)] = {
    //inferencePattern.fullPatternMatches

    val out = new Counter[(TableRow, TableRow)]

    val patternMatchIdxs = Range(0, inferencePattern.fullPatternMatches.length).par
    val numOfThreads = 10
    patternMatchIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numOfThreads))

    for (patternMatchIdx <- patternMatchIdxs) {

      val patternMatch = inferencePattern.fullPatternMatches(patternMatchIdx)
      val tuples = enumerationToPairwiseTuples(patternMatch, mode:Int)

      synchronized {
        out += tuples
      }
    }

    // Return
    out
  }


  def enumerationToPairwiseTuples(patternMatch:PatternMatchInfPat, mode:Int):Counter[(TableRow, TableRow)] = {
    val out = new Counter[(TableRow, TableRow)]
    val contentTags = Array("NN", "VB", "RB", "JJ", "CD")
    val stopwords = Set("is", "be", "kind", "means")


    for (i <- 0 until patternMatch.numSlots) {
      for (j <- 0 until patternMatch.numSlots) {
        if ((i != j) && (patternMatch.rowSlots(i).isDefined) && (patternMatch.rowSlots(j).isDefined)) {
          val rowI = patternMatch.rowSlots(i).get.row
          val rowJ = patternMatch.rowSlots(j).get.row

          val rowTextI = rowI.toStringText()
          val rowTextJ = rowJ.toStringText()

          // TODO: Check for good connection
          var hasConnection:Boolean = false

          var overlap = mutable.Set[Int]()
          var rowIWords = Set[String]()
          var rowJWords = Set[String]()
          if (mode == MODE_CONNECTION_DATA_COLS_ONLY) {
            rowIWords = rowI.getWordsLemmasDataFillCols(allowedTags = contentTags)
            rowJWords = rowJ.getWordsLemmasDataFillCols(allowedTags = contentTags)
          } else if (mode == MODE_CONNECTION_DATA_AND_FILL_COLS) {
            rowIWords = rowI.getWordsLemmasDataFillCols(allowedTags = contentTags)
            rowJWords = rowJ.getWordsLemmasDataFillCols(allowedTags = contentTags)
          }

          var intersection = rowIWords.intersect(rowJWords)
          intersection = intersection.diff(stopwords)

          /*
          println ("RowTextI: " + rowTextI)
          println ("RowTextJ: " + rowTextJ)
          println ("Intersection: " + intersection.mkString(", "))
          println ("")
           */
          if ((intersection.size > 0) || (mode == MODE_CONNECTION_NO_CONNECTION_REQUIRED)) {
            hasConnection = true
          }

          if (hasConnection) {
            //val tuple = (rowTextI, rowTextJ)
            val tuple = (rowI, rowJ)
            out.incrementCount(tuple)
          }

        }
      }
    }

    // Return
    out
  }



  /*
   * Generate IML runscript
   */

  // Generate an IML run script that will import an entire folder of IML patterns, enumerate them using the constraint satisfaction framework, and exit.
  def generateRunScript(folder:String, exportJSON:Boolean = true, maxMatches:Int):String = {
    val scriptCode = new ArrayBuffer[String]

    scriptCode.append("importFolder \"" + folder + "\"")
    scriptCode.append("")
    scriptCode.append("// Perform constraint satisfaction over the inference pattern and generate the HTML logfile output. ")

    scriptCode.append("setEnvironmentVariable(\"maxMatches\", \"" + maxMatches + "\")")   // Set the maximum number of matches in the enumeration

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("populateInfPatMatches")    // Run patterns

    scriptCode.append("incrementState")
    scriptCode.append("exportInfPatHTML()")
    if (exportJSON) scriptCode.append("exportInfPatJSON()")
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

  def makeProgressBarString(currentProgress:Double, maxProgress:Double, length:Int):String = {
    val os = new StringBuilder

    val proportion = currentProgress / maxProgress
    val lengthDone = math.floor(length.toDouble * proportion).toInt
    val lengthTodo = (length - lengthDone).toInt

    os.append("[")
    os.append("|" * lengthDone)
    os.append("-" * lengthTodo)
    os.append("] ")
    os.append((proportion*100).formatted("%3.1f") + "%")

    // Return
    os.toString()
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
    val scriptStr = generateRunScript(IMLInputPath, exportJSON = false, maxMatches = maxMatches)

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
    val inferencePatterns = interpreter.inferencePatterns.toArray
    //analysis(filteredQuestionsIn, inferencePatterns)

    var numEnumerations:Int = 0
    for (infPat <- inferencePatterns) numEnumerations += infPat.fullPatternMatches.length


    val filenameOut = "infPatTuplesOut-datacolconnection-numPat" + inferencePatterns.length + "-numEnum" + numEnumerations + ".tsv"
    exportEnumerationTuples(inferencePatterns, MODE_CONNECTION_DATA_COLS_ONLY, filenameOut)


  }



}

