package inferenceengine.iml.tests

import explanationgraph.visualization.TableStoreHtmlExport
import explanationgraph.{TableRow, TableStore}
import inferenceengine.iml.model._
import inferenceengine.iml.parser.IMLParser
import inferenceengine.iml.runtime.{IMLReader, Interpreter}
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
  * Created by peter on 1/29/19.
  */

object IMLTest {


  /*
   * Main
   */

  def main(args:Array[String]) = {
    val startTime = System.nanoTime()

    // Step 0: Parse command-line arguments
    val scriptFilename = "before10temp2/scripts/runme/runme.iml"
    val tablestorePath = "annotation/expl-unittesttablestore-current/tableindex.txt"
    val outputPath = "annotation/expl-unittesttablestore-current/output/"
    val cmdLineOpts = new CommandLineOptions(scriptFilename, tablestorePath, outputPath)

    var outputPrefix = cmdLineOpts.scriptFilename.substring(cmdLineOpts.scriptFilename.lastIndexOf("/"), cmdLineOpts.scriptFilename.lastIndexOf("."))
    if (outputPrefix.length > 0) outputPrefix += "."  // add dot at the end of the prefix, so filenames will be of the form (e.g.) scriptname.statespace.html

    // Step 1: Load tablestore
    val tablestore = new TableStore(cmdLineOpts.tablestoreIndex, twoLineHeader = false, inTableSubdir = true)

    // Step 1A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)

    // Step 2: Initialize parser and interpreter
    val parser = new IMLParser
    var interpreter = new Interpreter(tablestore, cmdLineOpts.outputPath, outputPrefix)

    // Step 3: Load from primary script
    // Step 3A: Find script filename from command line options
    println ("* Loading and parsing script '" + cmdLineOpts.scriptFilename + "'")

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
    // interpreter.setInferencePatternHighlight( findInfPatternsUsed(program.statements) )

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

  }

}


// Storage class for command line options
class CommandLineOptions(val scriptFilename:String, val tablestoreIndex:String, val outputPath:String) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("scriptFilename: " + scriptFilename + "\n")
    os.append("tablestoreIndex: " + tablestoreIndex + "\n")
    os.append("outputPath: " + outputPath + "\n")

    // Return
    os.toString()
  }
}
