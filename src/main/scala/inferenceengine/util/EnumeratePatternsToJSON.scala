package inferenceengine.util

import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import explanationgraph.TableStore
import inferenceengine.iml.runtime.IMLReader
import inferenceengineexperiments.EnumerateInferencePatterns

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport


object EnumeratePatternsToJSON {
  val VERSION = "1.0"
  val PROGRAM_TITLE = "WorldTree: EnumeratePatternsToJSON version " + VERSION

  /*
   * Prints the command line usage information to the console
   */
  def printUsage() {
    println (PROGRAM_TITLE)
    println ("")
    println ("Usage: ... -props myprops.properties")
    println ("")
  }


  def main(args:Array[String]): Unit = {
    // Step 1: Check that command line arguments were specified for running this program
    // e.g. " -props myprops.properties"
    if ((args.length == 0)) {
      printUsage()
      System.exit(1)
    }
    // Parse the command line arguments, and place this in a storage class called 'props'
    val props = StringUtils.argsToProperties(args)

    // Store the execution start time
    val startTime = System.nanoTime()

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

    // Step 3: Load IML Paths
    var IMLInputPath:String = ""
    if (props.getProperty("IMLInputPath", "") != "") {
      IMLInputPath = props.getProperty("IMLInputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLInputPath' property in properties file.")
    }

    var IMLOutputPath:String = ""
    if (props.getProperty("IMLOutputPath", "") != "") {
      IMLOutputPath = props.getProperty("IMLOutputPath", "")
    } else {
      throw new RuntimeException("ERROR: Unable to find 'IMLOutputPath' property in properties file.")
    }

    val maxMatches = StringUtils.getInt(props, "maxMatches", 10000)
    val numThreads = StringUtils.getInt(props, "numThreads", 2)
    val exportInfPatHTML = StringUtils.getBool(props, "exportInfPatHTML", false)
    val exportTableStoreHTML = StringUtils.getBool(props, "exportTableStoreHTML", false)

    // Find filenames to enumerate
    val filenamesIML = new ArrayBuffer[String]
    if (IMLInputPath.endsWith(".iml")) {
      // File
      filenamesIML.append(IMLInputPath)
    } else {
      // Folder
      filenamesIML.insertAll(0, IMLReader.findIMLFilesInFolder(IMLInputPath))
    }


    // Counter for Summary statistics
    val summaryStatistics = new Counter[String]
    var numPatternsProcessed:Int = 0
    var totalSolutions:Long = 0
    var timesLimitHit:Int = 0

    // Enumerate inference patterns, and save results to file.
    // This step is performed in parallel, for speed.
    val fileIndices = Range(0, filenamesIML.length).toArray.par

    fileIndices.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numThreads))

    for (fileIndex <- fileIndices) {
      val filename = filenamesIML(fileIndex)
      println ("* Filename " + fileIndex + " / " + filenamesIML.length + " : " + filename)

      // Step 4: Enumerate inference patterns, one file at a time
      val (infPats, enumerationOutStr) = EnumerateInferencePatterns.EnumerateInferencePatterns(tablestoreIndex, filename, IMLOutputPath, maxMatches = maxMatches, exportInfPatHTML = exportInfPatHTML, exportJSON = true, exportTableStoreHTML = exportTableStoreHTML, exportStateSpaceHTML = false)
      println (enumerationOutStr)

      // Calculate Summary statistics (synchronize across threads)
      synchronized {
        for (infPat <- infPats) {
          val summaryOut = infPat.getSummaryStatistics()
          summaryStatistics += summaryOut
          numPatternsProcessed += 1
          totalSolutions += infPat.fullPatternMatches.length
          if (infPat.fullPatternMatches.length >= maxMatches) timesLimitHit += 1
        }
      }

    }

    println ("--------------------------------------------------------------------------------------------")
    println ("")
    println ("EnumeratePatternsToJSON(): Execution Summary ")
    println ("")
    println ("--------------------------------------------------------------------------------------------")
    println ("               Pattern Summary Statistics (Average counts per pattern):")
    println ("--------------------------------------------------------------------------------------------")
    println ("")

    // Display summary statistics
    for (key <- summaryStatistics.keySet) {
      if (!key.endsWith("_COUNT")) {
        val normalized = summaryStatistics.getCount(key) / summaryStatistics.getCount(key + "_COUNT")
        println (("AVG " + key + ":  ").formatted("%38s")  + normalized.formatted("%3.1f") )
      }
    }

    println ("")
    println ("--------------------------------------------------------------------------------------------")
    println ("                              Execution Summary Statistics")
    println ("--------------------------------------------------------------------------------------------")
    println ("")

    // Compute the total execution time
    val endTime = System.nanoTime()
    val deltaTime = (endTime - startTime) / 1000000000L
    println ("Total Execution time:  ".formatted("%38s") + deltaTime + " second(s)  (threads: " + numThreads + ")")
    println ("Total Patterns Processed:  ".formatted("%38s") + numPatternsProcessed + " pattern(s)")
    println ("Total Solutions Generated:  ".formatted("%38s") + totalSolutions + " solution(s)")
    println ("Solution Limit Per Pattern:  ".formatted("%38s") + maxMatches + " solution(s)")
    println ("Number of times limit hit:  ".formatted("%38s") + timesLimitHit + " time(s)")

    println ("Pattern Input Path:  ".formatted("%38s") + IMLInputPath)
    println ("JSON Output Path:  ".formatted("%38s") + IMLOutputPath)
    println ("")

  }

}
