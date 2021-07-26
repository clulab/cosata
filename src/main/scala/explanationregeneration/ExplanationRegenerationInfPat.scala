package explanationregeneration

import java.io.{File, PrintWriter}
import java.util.Properties

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import edu.arizona.sista.struct.{Counter, Counters, Lexicon}
import edu.arizona.sista.utils.StringUtils
import data.question.ExamQuestionUtils.{convertToExplQuestions, filterQuestionsByFlags}
//import explanationexperiments.SummaryKnowledgeGrowth.calculateTableUseSummary
import explanationgraph.{TableRow, TableStore}

import data.tablerankings.ExternalTableRowRankings
import edu.arizona.sista.learning._
import inferenceengine.struct.{InferencePattern, PatternMatchInfPat}
import inferenceengineexperiments.EnumerateInferencePatterns
import inferenceengineexperiments.ExplanationRegenerationCeilingCalculator.{erccLexicon, getFilteredListOfCompletions, getRankedPatternScore, makeProgressBarString, patternMatchToUUIDSet}
import org.slf4j.LoggerFactory
import util.Histogram

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.control.Breaks._

/**
  * Created by peter on 3/10/2020.
  */

class ExplanationRegenerationInfPat {

}

object ExplanationRegenerationInfPat {
  val VERSION = "1.0"
  val PROGRAM_TITLE = "WorldTree: ExplanationRegenerationInfPat version " + VERSION
  val logger = LoggerFactory.getLogger(classOf[ExplanationRegenerationInfPat])



  /*
   * Prints the command line usage information to the console
   */
  def printUsage() {
    println (PROGRAM_TITLE)
    println ("")
    println ("Usage: ... -props myprops.properties")
    println ("")
  }


  /*
   * Main entry point
   */
  def main(args:Array[String]) {
    // Step 1: Check that command line arguments were specified for running this program
    // e.g. " -props myprops.properties"
    if ((args.length == 0)) {
      printUsage()
      System.exit(1)
    }
    // Parse the command line arguments, and place this in a storage class called 'props'
    val props = StringUtils.argsToProperties(args)


    // Step 2: Load the tablestore that we need to run the experiments

    // Step 2A: Find tablestore index filename from the properties list
    // The tablestore is loaded by referencing a file that includes a list of tables, called the tablestore index.
    // Load the name of the tablestore index file from the properties file.
    var tablestoreIndex:String = ""
    if (props.getProperty("tablestoreIndex", "") != "") {
      tablestoreIndex = props.getProperty("tablestoreIndex", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'tablestoreIndex' property in properties file.")
    }

    // Step 2B: Load the tablestore
    val tablestore = new TableStore(tablestoreIndex)

    //## Debug statements -- test usage of the tablestore
    println ( tablestore.tables( tablestore.UIDtoTableLUT("a5c9-d7a4-8421-bb2e") ).name )
    println ( tablestore.getRowByUID("a5c9-d7a4-8421-bb2e") )


    // Step 5: Load IML Paths
    // Step 5A: Initialize parser and interpreter
    var IMLOutputPath:String = ""
    if (props.getProperty("IMLOutputPath", "") != "") {
      IMLOutputPath = props.getProperty("IMLOutputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLOutputPath' property in properties file.")
    }

    // Step 5B: Retrieve the folder that the scripts reside in
    var IMLInputPath:String = ""
    if (props.getProperty("IMLInputPath", "") != "") {
      IMLInputPath = props.getProperty("IMLInputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLInputPath' property in properties file.")
    }


    // Load the maximum amount to enumerate any one inference pattern (or, set to a default value of 25000)
    val maxMatches = StringUtils.getInt(props, "maxMatches", 25000)




    // Example of enumerating inference patterns

    // Enumerate inference patterns
    val (infPats, enumerationOutStr) = EnumerateInferencePatterns.EnumerateInferencePatterns(tablestoreIndex, IMLInputPath, IMLOutputPath, maxMatches, exportInfPatHTML = false, exportJSON = false, exportTableStoreHTML = false, exportStateSpaceHTML = false)
    println (enumerationOutStr)



  }




}