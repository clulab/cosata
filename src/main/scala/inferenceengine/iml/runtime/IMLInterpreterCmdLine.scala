package inferenceengine.iml.runtime

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import explanationgraph.visualization.TableStoreHtmlExport
import explanationgraph.{TableRow, TableStore}
import inferenceengine.iml.model._
import inferenceengine.iml.parser.IMLParser
import inferenceengine.iml.visualization.IMLHtmlExport
import inferenceengine.struct.InferencePattern
import inferenceengine.struct._
import inferenceengine.util.LemmatizerSubstitutions

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.control.Breaks._
import inferenceengine.iml.states.InferenceState

/**
  * Created by peter on 1/10/19.
  */

object IMLInterpreterCmdLine {


  /*
   * Command line parsing
   */

  // Print the command line usage
  def printUsage() {
    println ("Usage: IMLInterpreterCmdLine --script <scriptFilename.iml> --tablestoreIndex </tables/tablestore.txt> --outputPath <outpath>")
  }


  def readCommandLine(args:Array[String]):CommandLineOptions = {
    // Parse command line arguments
    val scriptFilename = findArgumentParams("--script", 1, args)
    val tablestoreIndex = findArgumentParams("--tablestoreIndex", 1, args)
    var outputPath = findArgumentParams("--outputPath", 1, args)
    var timeLimit = findArgumentParams("--timeLimit", 1, args)
    var twolineHeader = findArgumentParams("--twolineHeader", 1, args)

    // Check for errors in command line arguments
    var hasError:Boolean = false
    if ((scriptFilename.isEmpty) || (scriptFilename.get.size != 1)) {
      println ("ERROR: No script filename defined. ")
      hasError = true
    }
    if ((tablestoreIndex.isEmpty) || (tablestoreIndex.get.size != 1)) {
      println ("ERROR: No tablestore index filename defined. ")
      hasError = true
    }
    if ((outputPath.isEmpty) || (outputPath.get.size != 1)) {
      // Default output path
      outputPath = Some(Array(""))
    }

    var timeLimitMSec:Long = 0
    if ((timeLimit.isEmpty) || (timeLimit.get.size != 1)) {
      // Default output path
      timeLimitMSec = 0
    } else {
      timeLimitMSec = (timeLimit.get(0).toInt) * 1000
    }

    var tlHeader:Boolean = false
    if (!twolineHeader.isEmpty) {
      if (twolineHeader.get(0).toLowerCase == "true") tlHeader = true
    }



    if (hasError) {
      printUsage()
      sys.exit(1)
    }

    // Pack into command line arguments storage class
    val cmdOpts = new CommandLineOptions(scriptFilename = scriptFilename.get(0), tablestoreIndex = tablestoreIndex.get(0), outputPath = outputPath.get(0), timeLimit = timeLimitMSec, twolineHeader = tlHeader)

    // Return
    cmdOpts
  }


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
    TimeLimit.setTimeStart()

    // Step 0: Parse command-line arguments
    val cmdLineOpts = readCommandLine(args)
    println ("Parsed command line options: ")
    println (cmdLineOpts.toString() )

    var lastSlashIdx = cmdLineOpts.scriptFilename.lastIndexOf("/")
    if (lastSlashIdx > 0) lastSlashIdx += 1
    var outputPrefix = cmdLineOpts.scriptFilename.substring(lastSlashIdx, cmdLineOpts.scriptFilename.lastIndexOf("."))
    if (outputPrefix.length > 0) outputPrefix += "."  // add dot at the end of the prefix, so filenames will be of the form (e.g.) scriptname.statespace.html
    //sys.exit(1)

    // Start timer (if applicable)
    if (cmdLineOpts.timeLimit > 0) {
      println ("Setting time limit to: " + cmdLineOpts.timeLimit + " msec")
      TimeLimit.setTimeLimitMSec(cmdLineOpts.timeLimit)
    }

    println ("outputPrefix: " + outputPrefix)
    println ("outputPath: " + cmdLineOpts.outputPath)

    // Step 1: Load tablestore
    //###
    //var pathAnnotation = "annotation/expl-minitablestore-current/"
    //val tablestore = new TableStore(pathAnnotation + "tableindex-mini.txt", twoLineHeader = true)
    // Before10Temp2
    //val tablestore = new TableStore(cmdLineOpts.tablestoreIndex, twoLineHeader = true, inTableSubdir = false)

    // Normal
    //val tablestore = new TableStore(cmdLineOpts.tablestoreIndex, twoLineHeader = false, inTableSubdir = true)
    val tablestore = new TableStore(cmdLineOpts.tablestoreIndex, twoLineHeader = cmdLineOpts.twolineHeader, inTableSubdir = true)

    /*
    //## Test -- write tablestore to file
    println ("Writing tablestore... ")
    val oos = new ObjectOutputStream(new FileOutputStream("tablestoreout.obj"))
    oos.writeObject(tablestore)
    oos.close()
    println ("Completed... ")

    println ("Reading Tablestore... ")
    val ois = new ObjectInputStream(new FileInputStream("tablestoreout.obj"))
    val stock = ois.readObject.asInstanceOf[TableStore]
    ois.close()
    println ("Completed... ")
     */

    // Step 1A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)

    // Step 2: Initialize parser and interpreter
    val parser = new IMLParser
    var interpreter = new Interpreter(tablestore, cmdLineOpts.outputPath, outputPrefix)

    // depricated -- now each InferencePattern stores it's own faux interpreter
    //##InferencePattern.InitializeInterpreter(tablestore)          // Special interpreter instance in InferencePattern must be initialized


    // Step 3: Load from primary script
    //###
    // Step 3A: Find script filename from command line options
    val scriptFilename = cmdLineOpts.scriptFilename
    println ("* Loading and parsing script '" + scriptFilename + "'")

    // Step 3B: Also insert extra code to the end of the script that exports logs
    val logInsertion = new StringBuffer
    logInsertion.append("\n")
    /*
    logInsertion.append("populateInfPatMatches\n")
    logInsertion.append("exportInfPatHTML(\"" + cmdLineOpts.outputPath + outputPrefix + "infpat.html" + "\")\n")
    logInsertion.append("exportTableStoreHTML(\"" + cmdLineOpts.outputPath + outputPrefix + "tablestore.html" + "\")\n")
    logInsertion.append("exportStateSpaceHTML(\"" + cmdLineOpts.outputPath + outputPrefix + "statespace.html" + "\")\n")
    */

    // Step 3C: Load and parse script
    val (program, successScript) = IMLReader.parseProgramFromFile(parser, scriptFilename, insertCodeAtEnd = logInsertion.toString())

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

    // Step 4: Run program

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

    // Compute the total execution time
    val endTime = System.nanoTime()
    val deltaTime = (endTime - startTime) / 1000000000L
    println ("Execution time: " + deltaTime + " seconds")

    println ("Running script complete (" + scriptFilename + ").")
    println ("Interpreter Result: " + result)
    if (result.exit) {
      println ("Exit.")
    } else if (result.success) {
      println ("---------------------------------------------------------")
      println ("Warning Message(s):")
      println (interpreter.warningStr.toString())
      println ("---------------------------------------------------------")
      println ("Success.")
    } else {
      println ("---------------------------------------------------------")
      println ("Warning Message(s):")
      println (interpreter.warningStr.toString())
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

    println ("Timelimit: " + TimeLimit.getDeltaTime())
  }

}


// Storage class for command line options
class CommandLineOptions(val scriptFilename:String, val tablestoreIndex:String, val outputPath:String, val timeLimit:Long,
                         val twolineHeader:Boolean) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("scriptFilename: " + scriptFilename + "\n")
    os.append("tablestoreIndex: " + tablestoreIndex + "\n")
    os.append("outputPath: " + outputPath + "\n")
    os.append("timeLimit: " + timeLimit + "\n")
    os.append("twolineHeader: " + twolineHeader + "\n")

    // Return
    os.toString()
  }
}