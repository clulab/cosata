package inferenceengine.iml.runtime

import java.io.File

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
import InferencePattern.{EXECUTIONMODE_AUTO, EXECUTIONMODE_AUTOREGEN, EXECUTIONMODE_NORMAL}
import inferenceengine.iml.states.InferenceState

/**
  * Created by user on 7/10/18.
  */
class IMLReader {

}



object IMLReader {
  // A set of filenames that have already been imported by the script reader (so that a given file is not imported more than once)
  var importedFiles = mutable.Set[String]()


  /*
   * Inference
   */

  // Extract the InferencePatterns described in the Program
  def generateInferencePatterns(program:Program, tablestore:TableStore):Array[InferencePattern] = {
    val out = new ArrayBuffer[InferencePattern]

    println ("* generateInferencePatterns: started...")

    // Get the script-level specifications of the inference patterns
    val infPatSpecs = program.infpatterns

    // For each script-level specification
    for (i <- 0 until infPatSpecs.length) {
      val infPatSpec = infPatSpecs(i)

      // Step 1: Name
      val name = infPatSpec.name
      val description = infPatSpec.description
      val pmDescription = infPatSpec.patternDescription.expr

      // Instance requirements
      var instReq = new InfInstanceReq(List())
      if (infPatSpec.requirements.isDefined) instReq = infPatSpec.requirements.get

      val compInfReq = infPatSpec.compReq

      // Constraints (Generic)
      val constraintsGeneric = infPatSpec.constraints.toArray

      // TODO: Constants


      // Step 3: Rows
      val numRows = infPatSpec.rows.length
      val rowPatterns = new Array[TableRowPattern](numRows)
      for (rowIdx <- 0 until numRows) {
        rowPatterns(rowIdx) = infPatSpec.rows(rowIdx).toTableRowPattern(tablestore)
      }


      // Execution mode
      var executionMode = infPatSpec.executionMode


      // TODO: Step 4: Code
      val codeblock = infPatSpec.code


      // Step 5: Generate
      out.append( InferencePattern.mkInferencePattern(name, description, pmDescription, executionMode, instReq, compInfReq, constraintsGeneric, Array.empty[TableRow], rowPatterns, codeblock, tablestore) )

      //println (i)
      println ("\n\n\n")
      println (" * Inference Pattern: " + out.last.name)
      println (out.last.toString(tablestore))
    }

    println ("* generateInferencePatterns: complete. " + out.size + " inference patterns generated. ")
    //##sys.exit(1)

    // Return
    out.toArray
  }



/*
  def doInference(program:Program, tablestore:TableStore): Unit = {
    // Inference patterns
    val inferencePatterns = generateInferencePatterns(program, tablestore)
  }
*/

  /*
   * Active Inference
   */
  /*
  def checkForActivePatterns(interpreter:Interpreter, tablestore:TableStore) = {
    println ("* checkForActivePatterns: Started...")
    // Find patterns
    for (i <- 0 until inferencePatterns.length) {
      inferencePatterns(i).findPatterns(tablestore)
    }

    // TODO: Incorporate this requirement matching -- right now we check for valid (existing instance/required instance) mappings, but don't use them.
    //## Check whether instances meet criterion
    for (i <- 0 until inferencePatterns.length) {
      //val results = inferencePatterns(i).getValidInstanceMatches( states.last.getAllInstances(), None, tablestore.lexicon )
      inferencePatterns(i).populatePatternMatchInstanceMatches( states.last.getAllInstances(), tablestore.lexicon )
    }

    println ("* checkForActivePatterns: Running interpreter for each inference pattern match...")
    // For each inference pattern, check the full pattern matches
    for (i <- 0 until inferencePatterns.length) {

      // Get the successful row matches/completions for this inference pattern
      val patternMatches = inferencePatterns(i).fullPatternMatches
      val codeblock = inferencePatterns(i).codeblock

      // For each patten match
      for (j <- 0 until patternMatches.length) {
        val patternMatch = patternMatches(j)

        // Retrieve the variable list
        val variableLUTs = patternMatch.variableValueLUT
        for (k <- 0 until variableLUTs.length) {
          val variableLUT = variableLUTs(k)

          // Interpreter
          println (" * Starting interpreter for Inference Pattern (" + inferencePatterns(i).name +") Match " + j + " / " + patternMatches.length + " [VariableAltSet: " + k +"]")
          interpreter.walkOneStep(codeblock, Some(variableLUT), debugDisplay = true)
          println ("")
          println ("---")
          println ("")


        }

      }

    }

    println ("* checkForActivePatterns: Completed...")
  }
  */

  def executePatternMatch(inferencePattern: InferencePattern, patternMatch: PatternMatchInfPat, remapping: Remap, interpreter: Interpreter): Unit = {
    // Get the successful row matches/completions for this inference pattern
    val codeblock = inferencePattern.codeblock

    // Retrieve the variable list
    val variableLUTs = patternMatch.variableValueLUT

    // Interpreter
    println(" * Starting interpreter for Inference Pattern (" + inferencePattern.name + ") patternMatch ")
    interpreter.addRemapLevel(remapping)
    interpreter.walkOneStep(codeblock, Some(variableLUTs), debugDisplay = true)
    interpreter.removeRemapLevel()

  }


  /*
   * IML Console
   */
  def consoleDisplayMessage() = {
    println ("Console Active")
  }

  def consoleGetInput():String = {
    print ("> ")
    val inputStr = scala.io.StdIn.readLine()

    // Return
    inputStr
  }


  /*
   * Parsing scripts
   */

  // Parse an IML script from a file
  def parseProgramFromFile(parser:IMLParser, filename:String, insertCodeAtEnd:String = ""):(Program, Boolean) = {
    // Read file and store as string
    val script = fileToString(filename) + insertCodeAtEnd

    // Extract any path information from the filename, so we can append it to any import statements
    var path = ""
    if (filename.lastIndexOf("/") > 0) {
      path = filename.substring(0, filename.lastIndexOf("/") ) + "/"
    }

    // Parse the program
    parseProgramFromString(parser, script, path)
  }

  // Parse an IML script from a string
  def parseProgramFromString(parser:IMLParser, script:String, importPath:String = "", debugDisplay:Boolean = false):(Program, Boolean) = {
    parser.parseAll(parser.program, script) match {
      // Success case
      case parser.Success(r, n) => r match {
        case program:Program => {

          // Succesfully parsed program
          if (debugDisplay) {
            println (r.toString)
            println ("Successfully parsed program. ")
          }

          val (programWithImports, successImport) = parseProgramImports(parser, program, importPath)
          if (successImport) {
            return (programWithImports, true)
          } else {
            return (new Program, false)
          }
        }
      }

      // Error cases
      case parser.Error(msg, n) => {
        println("Error: " + msg)
        // Return blank program
        return (new Program, false)
      }
      case parser.Failure(msg, n) => {
        println("Error: " + msg)
        // Return blank program
        return (new Program, false)
      }
      case _ => {
        println ("Unknown error")
        // Return blank program
        return (new Program, false)
      }
    }

  }


  def parseProgramImports(parser:IMLParser, progIn:Program, importPath:String):(Program, Boolean) = {
    // Check whether there are imports
    if (progIn.imports.length == 0) {
      return (progIn, true)
    }

    var prog:Program = progIn

    for (importFileOrFolder <- progIn.imports) {
      importFileOrFolder match {
        // Import a single file
        case importFile:ImportFile => {
          val filename = importPath + importFile.filename
          val (progAppended, result) = importProgFromFile(parser, prog, filename)
          if (result == false) return (new Program, false)      // Check for failure
          prog = progAppended                                   // If suggess, program becomes old program appended with new import
        }
        // Import all IML files in a given folder
        case importFolder:ImportFolder => {
          // Find a list of files to import
          val filesToImport = findIMLFilesInFolder(importFolder.path)

          // Import each file in turn
          for (filename <- filesToImport) {
            val (progAppended, result) = importProgFromFile(parser, prog, filename)
            if (result == false) return (new Program, false) // Check for failure
            prog = progAppended // If suggess, program becomes old program appended with new import
          }
        }
      }
    }

    // Return
    (prog, true)
  }

  private def importProgFromFile(parser:IMLParser, progIn:Program, filename:String):(Program, Boolean) = {
    var prog = progIn

    // Check that this file hasn't already been imported by another script
    if (!importedFiles.contains( filename )) {
      // Import file
      val (importedProg, success) = parseProgramFromFile(parser, filename)
      if (success) {
        prog = importedProg + prog
      } else {
        println ("ERROR: Failed to import file (" + filename + ")")
        return (new Program, false)
      }
      importedFiles += filename
    }

    // Return
    (prog, true)
  }

  /*
   * Supporting functions
   */
  def fileToString(filename:String) = {
    val file = Source.fromFile(filename)
    val str = file.mkString
    // Return
    str
  }

  // Recurseively finds a list of IML files in a given path
  def findIMLFilesInFolder(path:String):Array[String] = {
    val path1 = new File(path)
    if (!path1.isDirectory) println ("WARNING: Specified path is not a directory ('" + path + "')")
    val filesInFolder = findIMLFilesInFolderHelper(path1)

    val out = new ArrayBuffer[String]
    for (file <- filesInFolder) {
      val filename = file.getAbsolutePath
      if (filename.toLowerCase.endsWith(".iml")) {
        out.append(filename)
      }
    }

    // Return
    out.toArray
  }

  def findIMLFilesInFolderHelper(dir:File):Array[File] = {
    var files = ArrayBuffer[File]()
    if (dir.isDirectory) {
      files.insertAll(0, dir.listFiles)
      for (file <- files) {
        if (file.isDirectory) files.insertAll(files.size, findIMLFilesInFolderHelper(file))
      }
    }
    // Return
    files.toArray
  }


  /*
   * Main
   */

  def main(args:Array[String]) = {
    val enableConsole:Boolean = false

    // Step 1: Load tablestore
    var pathAnnotation = "annotation/expl-minitablestore-current/"
    //var pathAnnotation = "annotation/expl-minitablestore-export-2018-06-22-160637/"
    //var pathAnnotation = "annotation/expl-minitablestore-test/"
    val tablestore = new TableStore(pathAnnotation + "tableindex-mini.txt", twoLineHeader = true)
    println ( tablestore.tables( tablestore.UIDtoTableLUT("a0b2-a45f-01e1-4bf4") ).name )
    println ( tablestore.getRowByUID("a0b2-a45f-01e1-4bf4") )

    // Step 1A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)


    // Step 2: Initialize parser and interpreter
    val parser = new IMLParser
    var interpreter = new Interpreter(tablestore)

    // depricated -- now each InferencePattern stores it's own faux interpreter
    //## InferencePattern.InitializeInterpreter(tablestore)          // Special interpreter instance in InferencePattern must be initialized

    // Initialize inference state
    //states.append( new InferenceState )
    //interpreter.setStates(states.last, new InferenceState)      // TODO: Note (current, next) inference states -- currently next state is unimplmeneted, so left as a new instance here



    // Step 3: Load from primary script
    val scriptFilename = "scripts/test6a.iml"
    println ("* Loading and parsing script '" + scriptFilename + "'")
    val (program, successScript) = parseProgramFromFile(parser, scriptFilename)

    if (!successScript) {
      println ("Parse unsuccessful. ")
      sys.exit(1)
    }

    // Read the inference patterns from the primary script
    interpreter.addInferencePatterns( generateInferencePatterns(program, tablestore) )
    // exportInferencePatternMatching(tablestore)

    // Display the program
    println (program.toString())

    // Run the main statements from the primary script
    val result = interpreter.walkOneStep(program.statements, debugDisplay = true)
    interpreter.resetToGlobalVariableScope()
    println ("Interpreter Result: " + result)
    println ("Running script complete (" + scriptFilename + ").")

    //## Test
    // Run interpreter on inference patterns
    //checkForActivePatterns(interpreter, tablestore)


    //## DEBUG
    //println ("DEBUG: inferencePatterns(0).fullPatternMatches(0).instanceMatches.length: " + inferencePatterns(0).fullPatternMatches(0).instanceMatches.length)

    // Export inference patterns (note: if run after checkForActivePatterns(), will include instance matching information)
    //exportInferencePatternMatching(tablestore)

    // Export a debug dump of the tablestore to HTML
    //val tableStoreExporter = new TableStoreHtmlExport(tablestore)
    //tableStoreExporter.generateHTML("tablestore_export.html")


    //## TEST
    /*
    println ("\n---\n")
    println ("BEFORE:")
    val (program1, successScript1) = parseProgramFromString(parser, "printInstances")
    interpreter.walkOneStep(program1.statements, debugDisplay = true)

    println ("\n---\n")
    val altIdx = 0
    val inferencePattern = inferencePatterns(0)
    val patternMatch = inferencePattern.fullPatternMatches(0)

    //val remap = new Remap( inferencePatterns(0).fullPatternMatches(0).getInstanceRemap(altIdx, 0) )
    val infRemap = new RemapUni()
    //infRemap.addRemap("substance1", "obj1")
    infRemap.addRemap("substance1", "test1")
    val remap = new Remap( infRemap )

    val (isValid, validAltIdx) = patternMatch.checkIfValidRemap(infRemap)
    if (isValid) {
      println ("NOTE: Remap is valid")
    } else {
      println ("NOTE: Remal is invalid")
    }

    executePatternMatch(inferencePattern, patternMatch, remap, altIdx, interpreter )

    println ("\n---\n")
    println ("AFTER:")
    interpreter.walkOneStep(program1.statements, debugDisplay = true)
    */


    // Step 4: Initiate console
    if (!enableConsole) {
      println ("Exiting without console")
      sys.exit(1)
    }

    consoleDisplayMessage()

    breakable {
      while (true) {
        // Get user input (script) from console
        val consoleScript = consoleGetInput()
        if (consoleScript.trim().toLowerCase == "exit") {
          break()
        }

        // Parse script into program
        val (programConsole, successConsole) = parseProgramFromString(parser, consoleScript)

        if (successConsole) {
          // Only execute console statements -- assume for now that other constructs (e.g. inference patterns) are read in from the primary script
          val statementsConsole = programConsole.statements
          if (statementsConsole.length > 0) {
            val result = interpreter.walkOneStep(statementsConsole)
            if (!result.success) {
              interpreter.resetToGlobalVariableScope()
            }
            println ("Interpreter Result: " + result)
          }

        } else {

        }
      }
    }

    println ("Execution Complete.")

/*
    //## Test of instances
    val lexicon = tablestore.lexicon
    val instances = new ArrayBuffer[ObjectInstance]
    val kindOf = Array( lexicon.add("substance") )

    instances.append( new ObjectInstance("name1", kindOf, tablestore) )

    instances(0).insertProperties( kindOf )

    val value = new PropertyValue
    value.setDiscrete("solid")
    instances(0).setProperty("state of matter", value)

    println ("Instance Test: ")
    println ( instances(0).toString() )

    sys.exit(1)
*/

  }

}