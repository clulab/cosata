package inferenceengineexperiments

import explanationgraph.{CombinationIterator, TableRow, TableStore}
import inferenceengine.iml.model._
import inferenceengine.iml.parser.IMLParser
import inferenceengine.iml.runtime.{IMLReader, Interpreter}
import inferenceengine.struct.InferencePattern
import inferenceengine.util.LemmatizerSubstitutions
import inferenceengineexperiments.ExplanationRegenerationCeilingCalculator.generateRunScript

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Utility to enumerate all IML inference patterns in an input directory, for use in downstream processing tasks
  * Created by peter on 3/10/2020.
  */

object EnumerateInferencePatterns {

  def EnumerateInferencePatterns(tablestoreIndex:String, IMLInputPath:String, IMLOutputPath:String, maxMatches:Int = 25000, exportInfPatHTML:Boolean = true, exportJSON:Boolean = false, exportTableStoreHTML:Boolean = true, exportStateSpaceHTML:Boolean = true):(Array[InferencePattern], String) = {
    val startTime = System.nanoTime()

    // Step 2: Load the tablestore that we need to run the experiments
    val tablestore = new TableStore(tablestoreIndex, twoLineHeader = false, inTableSubdir = true)

    // Step 3A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)

    // Step 4: Initialize parser and interpreter
    val parser = new IMLParser
    var interpreter = new Interpreter(tablestore, IMLOutputPath, outputPrefix = "")

    // Step 5: Load IML files from specified folder
    // Step 5A: Automatically generate an IML runscript that will import that folder of IML patterns and enumerate them.
    //val scriptStr = generateRunScript(IMLInputPath, IMLOutputPath)
    val scriptStr = generateRunScript(IMLInputPath, IMLOutputPath, maxMatches = maxMatches, exportInfPatHTML = exportInfPatHTML, exportJSON = exportJSON, exportTableStoreHTML = exportTableStoreHTML, exportStateSpaceHTML = exportStateSpaceHTML)

    // Step 5B: Load and parse script
    val (program, successScript) = IMLReader.parseProgramFromString(parser, scriptStr)

    if (!successScript) {
      println ("Parse unsuccessful. ")
      sys.exit(1)
    }

    // Read the inference patterns from the primary script
    interpreter.addInferencePatterns( IMLReader.generateInferencePatterns(program, tablestore) )
    interpreter.setInferencePatternHighlight( findInfPatternsUsed(program.statements) )

    // Display the program
    // println (program.toString())

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
    val os = new StringBuilder
    val endTime = System.nanoTime()
    val deltaTime = (endTime - startTime) / 1000000000L
    os.append ("Execution time: " + deltaTime + " seconds" + "\n")

    os.append ("Running script complete (IMLInputPath: " + IMLInputPath + ")." + "\n")
    os.append ("Interpreter Result: " + result + "\n")
    if (result.exit) {
      os.append ("Exit." + "\n")
    } else if (result.success) {
      os.append ("Success." + "\n")
    } else {
      os.append ("---------------------------------------------------------" + "\n")
      os.append ("Error Message: " + "\n")
      os.append (interpreter.lastErrorStr.toString() + "\n")
      os.append ("---------------------------------------------------------" + "\n")
      os.append ("Statement: " + "\n")
      if (interpreter.lastStatement.isDefined) {
        os.append (interpreter.lastStatement + "\n")
      } else {
        os.append (" not defined" + "\n")
      }
      os.append ("---------------------------------------------------------" + "\n")
      os.append ("Failure." + "\n")
    }
    os.append ("Execution Complete." + "\n")


    // Step 7: Output
    val inferencePatterns = interpreter.inferencePatterns.toArray


    // Return
    (inferencePatterns, os.toString)
  }



  /*
   * Generate IML runscript
   */
  // Generate an IML run script that will import an entire folder of IML patterns, enumerate them using the constraint satisfaction framework, and exit.
  def generateRunScript(fileOrFolderName:String, outputPath:String, maxMatches:Int, exportInfPatHTML:Boolean = true, exportJSON:Boolean = false, exportTableStoreHTML:Boolean = true, exportStateSpaceHTML:Boolean = true):String = {
    val scriptCode = new ArrayBuffer[String]
    var jsonFilename = outputPath + "/" + fileOrFolderName.split("/").last + ".json"
    val htmlFilename = outputPath + "/" + fileOrFolderName.split("/").last + ".infpat.html"

    if (fileOrFolderName.endsWith(".iml")) {
      scriptCode.append("import \"" + fileOrFolderName + "\"")

    } else {
      // Assume folder
      scriptCode.append("importFolder \"" + fileOrFolderName + "\"")
    }

    scriptCode.append("")
    scriptCode.append("// Perform constraint satisfaction over the inference pattern and generate the HTML logfile output. ")

    scriptCode.append("setEnvironmentVariable(\"maxMatches\", \"" + maxMatches + "\")")   // Set the maximum number of matches in the enumeration

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("incrementState")

    scriptCode.append("populateInfPatMatches")    // Run patterns

    /*
    scriptCode.append("incrementState")

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("incrementState")

    scriptCode.append("populateInfPatMatches")    // Run patterns
     */

    if (exportInfPatHTML) scriptCode.append("exportInfPatHTML(\"" + htmlFilename + "\")")
    if (exportJSON) scriptCode.append("exportInfPatJSON(\"" + jsonFilename + "\")")

    if (exportTableStoreHTML) scriptCode.append("exportTableStoreHTML()")
    if (exportStateSpaceHTML) scriptCode.append("exportStateSpaceHTML()")

    val scriptStr = scriptCode.mkString("\n")

    // Return
    scriptStr
  }

  // Generate an IML run script that will import an single IML file or an entire folder of IML patterns, enumerate them using the constraint satisfaction framework, and exit.
  def generateRunScript(fileOrFolderName:String, outputPath:String):String = {
    val scriptCode = new ArrayBuffer[String]
    var jsonFilename = outputPath + "/" + fileOrFolderName.split("/").last + ".json"

    if (fileOrFolderName.endsWith(".iml")) {
      scriptCode.append("import \"" + fileOrFolderName + "\"")

    } else {
      // Assume folder
      scriptCode.append("importFolder \"" + fileOrFolderName + "\"")
    }

    scriptCode.append("")
    scriptCode.append("// Perform constraint satisfaction over the inference pattern and generate the HTML logfile output. ")

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("populateInfPatMatches")    // Run patterns

    scriptCode.append("incrementState")
    scriptCode.append("exportInfPatHTML()")
    scriptCode.append("exportInfPatJSON(\"" + jsonFilename + "\")")
    scriptCode.append("exportTableStoreHTML()")
    scriptCode.append("exportStateSpaceHTML()")

    val scriptStr = scriptCode.mkString("\n")

    // Return
    scriptStr
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


}
