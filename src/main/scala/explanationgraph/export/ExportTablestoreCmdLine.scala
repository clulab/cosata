package explanationgraph.export

import java.io.PrintWriter

import explanationgraph.TableStore
import inferenceengine.util.LemmatizerSubstitutions

import scala.collection.mutable.ArrayBuffer

class ExportTablestoreCmdLine {

}

object ExportTablestoreCmdLine {

  /*
 * Command line parsing
 */

  // Print the command line usage
  def printUsage() {
    println ("Usage: ExportTablestoreCmdLine --tablestoreIndex </tables/tablestore.txt> --outputTablestore </out/>" )
  }


  def readCommandLine(args:Array[String]):CommandLineOptions = {
    // Parse command line arguments
    //val scriptFilename = findArgumentParams("--script", 1, args)
    val tablestoreIndex = findArgumentParams("--tablestoreIndex", 1, args)
    var outputTablestorePath = findArgumentParams("--outputTablestore", 1, args)

    // Check for errors in command line arguments
    var hasError:Boolean = false
    /*
    if ((scriptFilename.isEmpty) || (scriptFilename.get.size != 1)) {
      println ("ERROR: No script filename defined. ")
      hasError = true
    }
    */

    if ((tablestoreIndex.isEmpty) || (tablestoreIndex.get.size != 1)) {
      println ("ERROR: No tablestore index filename defined. ")
      hasError = true
    }
    if ((outputTablestorePath.isEmpty) || (outputTablestorePath.get.size != 1)) {
      // Default output path
      outputTablestorePath = Some(Array(""))
    }


    if (hasError) {
      printUsage()
      sys.exit(1)
    }

    // Pack into command line arguments storage class
    val cmdOpts = new CommandLineOptions(tablestoreIndex = tablestoreIndex.get(0), outputTablestorePath = outputTablestorePath.get(0))

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
   * Export tablestore to JSON
   */
  def exportTablestoreToJSON(tablestore:TableStore):String = {
    val os = new StringBuilder

    os.append("{" + "\n")

    val tableNames = tablestore.getTableNames()
    val tableJSON = new ArrayBuffer[String]()
    for (i <- 0 until tableNames.length) {
      val table = tablestore.getTableByName(tableNames(i)).get

      val tableJSONStr = new StringBuilder
      tableJSONStr.append("\t\"" + table.name + "\": [\n")

      val rowJSON = new ArrayBuffer[String]()
      for (row <- table.rows) {
        rowJSON.append("\t\t" + row.exportToJSON() )
      }
      tableJSONStr.append( rowJSON.mkString(",\n") + "\n" )

      tableJSONStr.append("\t]")
      tableJSON.append(tableJSONStr.toString)
    }

    os.append( tableJSON.mkString(",\n") + "\n" )

    os.append("}" + "\n")

    // Return
    os.toString()
  }

  def exportTablestoreToJSONFile(tablestore:TableStore, filenameOut:String) {
    val pw = new PrintWriter(filenameOut)
    pw.println(exportTablestoreToJSON(tablestore))
    pw.close()
  }



  /*
   * Main function
   */

  def main(args:Array[String]) = {
    val startTime = System.nanoTime()


    // Step 0: Parse command-line arguments
    val cmdLineOpts = readCommandLine(args)
    println ("Parsed command line options: ")
    println (cmdLineOpts.toString() )

    // Step 1: Load tablestore
    val tablestore = new TableStore(cmdLineOpts.tablestoreIndex, twoLineHeader = false, inTableSubdir = true)

    // Step 1A: Load lemmatizer substitutions
    LemmatizerSubstitutions.load("annotation/lemmatizerSubstitutions.lemma.tsv", tablestore.lexicon)


    // Step 2: Export tablestore

    val jsonStr = exportTablestoreToJSON(tablestore)

    println (jsonStr)

    exportTablestoreToJSONFile(tablestore, cmdLineOpts.outputTablestorePath + "/" + "tablestore.json")


    // Compute the total execution time
    val endTime = System.nanoTime()
    val deltaTime = (endTime - startTime) / 1000000000L
    println ("Execution time: " + deltaTime + " seconds")
  }


}


// Storage class for command line options
class CommandLineOptions(val tablestoreIndex:String, val outputTablestorePath:String) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("tablestoreIndex: " + tablestoreIndex + "\n")
    os.append("outputTablestorePath: " + outputTablestorePath + "\n")

    // Return
    os.toString()
  }
}