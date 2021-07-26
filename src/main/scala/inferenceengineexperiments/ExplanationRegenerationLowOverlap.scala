package inferenceengineexperiments


import java.io.{File, PrintWriter}

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
import scala.io.Source
import scala.util.control.Breaks._
import inferenceengine.iml.states.InferenceState
import util.Histogram

/**
  * Produces an analysis that shows which questions (and question classes) have poor coverage with inference patterns generated from Synchronicity,
  * to help generate inference patterns in a targetted way for the subset of questions where they're most useful.
  * Created by peter on 1/22/19.
  */

object ExplanationRegenerationLowOverlap {

  /*
   * Analyses
   */
  def analysis (questions:Array[MCExplQuestion], populatedPatterns:Array[InferencePattern], tablestore:TableStore, patterns:Array[InfPatUUIDs], maxCombinations:Int = 1): String = {
    val os = new StringBuilder

    val questionsExceedCombinationLimit = new ArrayBuffer[MCExplQuestion]


    //val roleFilter = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    val roleFilter = Array("CENTRAL", "GROUNDING")

    val hists = new Array[Counter[Int]](maxCombinations)
    for (i <- 0 until hists.length) hists(i) = new Counter[Int]

    val histsPercent = new Array[Histogram](maxCombinations)
    for (i <- 0 until histsPercent.length) histsPercent(i) = new Histogram(name = i + " Pattern(s) Combined")


    var averageOverlaps = Array.fill[Double](maxCombinations)(0.0)
    val questionOverlaps = Array.fill[Double](questions.length)(0.0)
    val questionLeftoverUUIDs = Array.fill[Set[String]](questions.length)(Set())

    // For each question
    for (i <- 0 until questions.length) {
      val question = questions(i)
      val qExplUUIDs = question.getExplanationUUIDs(roleFilter)

      println ("Question " + i + ": ")
      println (question.toString())

      val shortlist = getListOfCompletions(question, populatedPatterns)

      println ("")

      println ("Shortlist has " + shortlist.length + " completions. ")

      for (patternsToCombine <- 1 to maxCombinations) {
        println ("Creating combinations of " + patternsToCombine + " patterns... ")
        val combinations = mkTopCombinations(question, shortlist, numPatternsToCombine = patternsToCombine)

        println("Top combinations returned: " + combinations.size)

        println ("Sorting... ")
        val sorted = combinations.sortBy(-_.getIntersectionSize())

        println ("Evaluating... ")

        var maxSize = 0
        var maxPercentOverlap = 0.0
        var leftoverUUIDs = Set[String]()
        if (sorted.length > 0) {
          maxSize = sorted(0).getIntersectionSize()
          maxPercentOverlap = sorted(0).getPercentage(qExplUUIDs)
          leftoverUUIDs = qExplUUIDs.diff(sorted(0).rowsIntersection)
        } else {
          leftoverUUIDs = qExplUUIDs
        }

        hists(patternsToCombine-1).incrementCount(maxSize)
        histsPercent(patternsToCombine-1).addData(maxPercentOverlap, tag="")

        averageOverlaps(patternsToCombine-1) += maxPercentOverlap

        questionOverlaps(i) = maxPercentOverlap
        questionLeftoverUUIDs(i) = leftoverUUIDs
      }

      println ("")
      println ("------------------------------------------------------------------------------")
      println ("")


    }


    // Step 8: Inference pattern summary
    println ("")
    os.append ("Summary of Inference Patterns (and Number of Matches) in Analysis:" + "\n")
    var sumPatternEnumerations:Int = 0
    for (i <- 0 until populatedPatterns.length) {
      val name = populatedPatterns(i).name
      val numPatternMatches = populatedPatterns(i).fullPatternMatches.length
      os.append (i + "\t" + numPatternMatches.formatted("%10s") + "\t" + name + "\n")
      sumPatternEnumerations += numPatternMatches
    }
    os.append ("\n")
    os.append("Total pattern enumerations: " + sumPatternEnumerations + "\n")
    os.append ("\n")

    // Calculate averages
    for (i <- 0 until averageOverlaps.length) averageOverlaps(i) = averageOverlaps(i) / questions.length.toDouble


    os.append ("Histogram: " + "\n")
    // Header
    os.append("Overlap".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (i + " pattern(s)").formatted("%20s")
      os.append(str)
    }
    os.append ("\n")
    // Data
    for (j <- 0 until 15) {
      os.append(j.formatted("%20s"))
      for (i <- 1 to maxCombinations) {
        val count = hists(i-1).getCount(j)
        val str = count.formatted("%20s")
        os.append(str)
      }
      os.append ("\n")
    }
    os.append ("\n")


    // Average percentage overlaps
    os.append("AvgOverlap".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageOverlaps(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append("\n")
    os.append("\n")

    // Percentage histograms
    for (i <- 1 to maxCombinations) {
      os.append(histsPercent(i-1).toString() + "\n")
    }

    // Question overlaps
    for (truncationLevel <- 1 until 6) {
      val qcHistLow = new Counter[String]
      val qcHistMed = new Counter[String]
      val qcHistHigh = new Counter[String]
      val leftoverUUIDsLow = new mutable.HashMap[String, Counter[String]]()
      val leftoverUUIDsMed = new mutable.HashMap[String, Counter[String]]()
      val leftoverUUIDsHigh = new mutable.HashMap[String, Counter[String]]()

      for (i <- 0 until questions.length) {
        val question = questions(i).question
        val topics = question.topic
        val maxOverlap = questionOverlaps(i)
        val leftovers = questionLeftoverUUIDs(i)

        if (maxOverlap < 0.33) {
          for (topic <- topics) {
            val topicTruncated = qcLabelTruncation(topic, truncationLevel)
            qcHistLow.incrementCount(topicTruncated)

            var leftoverSet = new Counter[String]()
            if (leftoverUUIDsLow.contains(topicTruncated)) leftoverSet = leftoverUUIDsLow.get(topicTruncated).get
            for (uuid <- leftovers) leftoverSet.incrementCount(uuid)
            leftoverUUIDsLow(topicTruncated) = leftoverSet
          }
        } else if (maxOverlap < 0.66) {
          for (topic <- topics) {
            val topicTruncated = qcLabelTruncation(topic, truncationLevel)
            qcHistMed.incrementCount(topicTruncated)

            var leftoverSet = new Counter[String]()
            if (leftoverUUIDsMed.contains(topicTruncated)) leftoverSet = leftoverUUIDsMed.get(topicTruncated).get
            for (uuid <- leftovers) leftoverSet.incrementCount(uuid)
            leftoverUUIDsMed(topicTruncated) = leftoverSet
          }
        } else if (maxOverlap <= 1.00) {
          for (topic <- topics) {
            val topicTruncated = qcLabelTruncation(topic, truncationLevel)
            qcHistHigh.incrementCount(topicTruncated)

            var leftoverSet = new Counter[String]()
            if (leftoverUUIDsHigh.contains(topicTruncated)) leftoverSet = leftoverUUIDsHigh.get(topicTruncated).get
            for (uuid <- leftovers) leftoverSet.incrementCount(uuid)
            leftoverUUIDsHigh(topicTruncated) = leftoverSet
          }
        }
      }

      val maxDisplay:Int = 30
      os.append ("---------------------------------------------------------------------------" + "\n")
      os.append ("QC Label Truncation Level: " + truncationLevel + "\n")
      os.append ("---------------------------------------------------------------------------" + "\n")
      os.append ("\nLow Scoring (0.00 - 0.33)" + "\n")
      val sortedLow = qcHistLow.sorted(descending = true)
      for (i <- 0 until math.min(sortedLow.length, maxDisplay)) {
        val qcLabel = sortedLow(i)._1
        val count = sortedLow(i)._2
        os.append (i + ":\t" + count + "\t" + qcLabel + "\n")
        val leftoverUUIDsSorted = leftoverUUIDsLow.get(qcLabel).getOrElse(new Counter[String]()).sorted(descending = true)
        os.append("\t\tMissing Rows in top pattern match: " + "\n")
        for (uuidPair <- leftoverUUIDsSorted) {
          val uuid = uuidPair._1
          val freq = uuidPair._2
          val text = tablestore.getRowByUID(uuid).toStringText()
          os.append ("\t\t" + freq + "\t" + uuid + "\t" + text + "\n")
        }

        val topPatterns = getTopPatterns(leftoverUUIDsLow.get(qcLabel).getOrElse(new Counter[String]()), patterns)
        os.append( displayTopPatterns(topPatterns) + "\n")
      }

      os.append ("\nMedium Scoring (0.33 - 0.66)" + "\n")
      val sortedMed = qcHistMed.sorted(descending = true)
      for (i <- 0 until math.min(sortedMed.length, maxDisplay)) {
        val qcLabel = sortedMed(i)._1
        val count = sortedMed(i)._2
        os.append (i + ":\t" + count + "\t" + qcLabel + "\n")
        val leftoverUUIDsSorted = leftoverUUIDsMed.get(qcLabel).getOrElse(new Counter[String]()).sorted(descending = true)
        os.append("\t\tMissing Rows in top pattern match: " + "\n")
        for (uuidPair <- leftoverUUIDsSorted) {
          val uuid = uuidPair._1
          val freq = uuidPair._2
          val text = tablestore.getRowByUID(uuid).toStringText()
          os.append ("\t\t" + freq + "\t" + uuid + "\t" + text + "\n")
        }

        val topPatterns = getTopPatterns(leftoverUUIDsMed.get(qcLabel).getOrElse(new Counter[String]()), patterns)
        os.append( displayTopPatterns(topPatterns) + "\n")
      }

      os.append ("\nHigh Scoring (0.66 - 1.00)" + "\n")
      val sortedHigh = qcHistHigh.sorted(descending = true)
      for (i <- 0 until math.min(sortedHigh.length, maxDisplay)) {
        val qcLabel = sortedHigh(i)._1
        val count = sortedHigh(i)._2
        os.append (i + ":\t" + count + "\t" + qcLabel + "\n")
        val leftoverUUIDsSorted = leftoverUUIDsHigh.get(qcLabel).getOrElse(new Counter[String]()).sorted(descending = true)
        os.append("\t\tMissing Rows in top pattern match: " + "\n")
        for (uuidPair <- leftoverUUIDsSorted) {
          val uuid = uuidPair._1
          val freq = uuidPair._2
          val text = tablestore.getRowByUID(uuid).toStringText()
          os.append ("\t\t" + freq + "\t" + uuid + "\t" + text + "\n")
        }

        val topPatterns = getTopPatterns(leftoverUUIDsHigh.get(qcLabel).getOrElse(new Counter[String]()), patterns)
        os.append( displayTopPatterns(topPatterns) + "\n")
      }

      os.append ("\n")
    }



    os.append ("roleFilter: " + roleFilter.mkString(", ") + "\n")

    // Return
    os.toString()
  }



  def mkTopCombinations(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[String], Set[String])], numPatternsToCombine:Int):Array[CompletionCombinationEvaluationStr] = {
    val out = new ArrayBuffer[CompletionCombinationEvaluationStr]
    val MAX_SIZE = 10000

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
        out.append(new CompletionCombinationEvaluationStr(completionList, completionUUIDsSum.toSet, intersection.toSet, -1))
        maxIntersectionSize = intersection.size

        // Limit the number of combinations returned (to save memory)
        if (out.size > MAX_SIZE) {
          val topSorted = out.sortBy(-_.getIntersectionSize()).slice(0, 1000)
          out.clear()
          out.insertAll(0, topSorted)
        }
      }
    }

    // Return
    out.toArray
  }

  def findTopCombination(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[String], Set[String])], numPatternsToCombine:Int):Array[CompletionCombinationEvaluationStr] = {
    println (" * findTopCombination(): Started... (numPatternsToCombine: " + numPatternsToCombine + ")")
    var out:Option[CompletionCombinationEvaluationStr] = None
    val MAX_SIZE = 10000

    val qExplUUIDs = question.getExplanationUUIDs()
    val qExplUUIDsSize = qExplUUIDs.size

    val maxValues = Array.fill[Int](numPatternsToCombine)(shortlist.size)
    val iter = new CombinationIterator(maxValues)

    var maxIntersectionSize:Int = 0
    var maxExtraSize:Int = 0

    var count:Long = 0
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
      val numExtra = completionUUIDsSum.diff(qExplUUIDs).size

      // TODO: Only store if the match has a high number of intersections, to save memory?

      // Store evaluation
      // **NOTE**: For this ceiling analysis, to save time/memory, only store those overlaps that are at least as large as the maximum overlap previously encountered for this question
      // ** This means the list returned will not be complete, so not useful for downstream tasks other than ceiling performance calculation. **
      if ((intersection.size > maxIntersectionSize) || ((intersection.size == maxIntersectionSize) && (numExtra < maxExtraSize))) {
        out = Some(new CompletionCombinationEvaluationStr(completionList, completionUUIDsSum.toSet, intersection.toSet, numExtra))
        maxIntersectionSize = intersection.size
        maxExtraSize = numExtra
      }

      count += 1
      if (count % 100000 == 0) {
        val percent = ((count.toDouble / iter.size.toDouble)*100).formatted("%3.1f") + "%"
        print(percent + "  ")
      }
      if (count % 2000000 == 0) {
        println ("")
      }
    }

    // Return
    if (out.isDefined) {
      Array(out.get)
    } else {
      Array.empty[CompletionCombinationEvaluationStr]
    }
  }

  // Get a list of inference pattern completions that have non-zero overlap with the explanation for a given question
  def getListOfCompletions(question:MCExplQuestion, populatedPatterns:Array[InferencePattern]):Array[(PatternMatchInfPat, Set[String], Set[String])] = {
    val explUUIDs = question.getExplanationUUIDs()

    // For each inference pattern, check that pattern's completions for overlap with this explanation.
    // Add any completion with non-zero overlap to a shortlist.
    val patternsWithNonZeroOverlap = new ArrayBuffer[(PatternMatchInfPat, Set[String], Set[String])]()       // Shortlist
    for (i <- 0 until populatedPatterns.length) {
      val completions = populatedPatterns(i).fullPatternMatches
      for (completion <- completions) {
        // Convert from pattern match completion storage class to a set of UUIDs representing that completion
        val uuids = patternMatchToUUIDSet(completion)
        // Check for overlap between completion and question explanation
        val intersection = explUUIDs.intersect(uuids)
        // If overlap, store in shortlist
        if (intersection.size > 0) {
          patternsWithNonZeroOverlap.append( (completion, uuids, intersection) )
        }
      }
    }

    // Return
    patternsWithNonZeroOverlap.toArray
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
    scriptCode.append("exportTableStoreHTML()")
    scriptCode.append("exportStateSpaceHTML()")

    val scriptStr = scriptCode.mkString("\n")

    // Return
    scriptStr
  }


  /*
   * Loading Synchronicity inference pattern annotation
   */
  // Loads a pattern, returns a set of UUIDs in that pattern (main + hint rows)
  def loadSynchronicityInferencePatternTSV(filename:String, name:String):InfPatUUIDs = {
    val delim = "\t"
    val lines = io.Source.fromFile(filename, "UTF-8").getLines().toArray


    // Parse ratings
    val ratingIdx = 1
    val ratingFields = lines(ratingIdx).split("delim")
    var ratingsStr:String = ""
    for (i <- 0 until ratingFields.length) {
      if (ratingFields(i).toLowerCase == "true") {
        ratingsStr += i.toString + " "
      }
    }

    // Parse header
    val headerIdx = 2
    val header = lines(headerIdx).split(delim)
    val uuidColIdx = header.indexOf("UID")
    val hintRowColIdx = header.indexOf("hintRowUUIDs")

    // Parse data
    val uuidsOut = mutable.Set[String]()
    for (lineIdx <- 3 until lines.length) {
      val line = lines(lineIdx)
      val fields = line.split(delim)
      val uuidField = fields(uuidColIdx)
      val hintRowField = fields(hintRowColIdx)

      // Add main UUID
      uuidsOut.add(uuidField)
      for (hintRowUUID <- hintRowField.split(" ")) {
        val uuid = hintRowUUID.trim()
        if (uuid.length > 0) {
          uuidsOut.add(uuid)
        }
      }
    }

    // Return
    new InfPatUUIDs(name = name, uuids = uuidsOut.toSet, rating = ratingsStr.toString)
  }

  def loadAllSynchronicityPatterns(dir:String):Array[InfPatUUIDs] = {
    println (" * loadAllSynchronicityPatterns: Started...")
    val out = new ArrayBuffer[InfPatUUIDs]()
    val files = getListOfFiles(dir)

    for (file <- files) {
      val filename = file.getAbsolutePath
      // println ("filename: " + filename)
      if (filename.endsWith(".tsv")) {
        val patternName = file.getName
        val pattern = loadSynchronicityInferencePatternTSV(filename, name = patternName)
        // println (patternName + "\t" + patternUUIDs)

        out.append(pattern)
      }

    }

    println (" * loadAllSynchronicityPatterns: Completed (loaded " + out.length + " patterns)")

    // Return
    out.toArray
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
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

  def getTopPatterns(uuidsIn:Counter[String], patterns:Array[InfPatUUIDs]):Array[(Double, InfPatUUIDs)] = {
    val patternScores = new ArrayBuffer[(Double, InfPatUUIDs)]

    for (pattern <- patterns) {
      var score:Double = 0
      for (uuid <- pattern.uuids) {
        val count = uuidsIn.getCount(uuid)
        score += count
      }
      patternScores.append( (score, pattern) )
    }

    val sorted = patternScores.sortBy(- _._1)

    // Return
    sorted.toArray
  }

  def displayTopPatterns(in:Array[(Double, InfPatUUIDs)]):String = {
    val maxDisplay:Int = 15
    val os = new StringBuilder

    os.append("\t\t\tTop patterns with coverage of missing rows:\n")
    var numOutput:Int = 0
    for (i <- 0 until math.min(in.length, maxDisplay)) {
      val score = in(i)._1
      val pattern = in(i)._2
      val name = pattern.name
      val rating = pattern.rating

      if (score > 0) {
        os.append("\t\t\t" + score.formatted("%3.3f") + "\t" + rating + "\t" + name + "\n")
        numOutput += 1
      }
    }

    if (numOutput == 0) {
      os.append("\t\t\tNo patterns with overlap found.\n")
    }

    // Return
    os.toString
  }

  // Truncate a QC label to a specific level
  def qcLabelTruncation(label:String, truncateToLevel:Int):String = {
    val split = label.split("_")
    val slice = split.slice(0, truncateToLevel)
    // Return
    slice.mkString("_")
  }



  /*
   * Main
   */

  def main(args:Array[String]) = {
    val startTime = System.nanoTime()

    // Step 1: Parse the command line arguments, and place this in a storage class called 'props'
    val props = StringUtils.argsToProperties(args)

    // Load Synchronicity patterns
    var patterns = Array.empty[InfPatUUIDs]
    val patternsDir = props.getProperty("synchronicityPatternsDir", "")
    if (patternsDir.length > 0) {
      patterns = loadAllSynchronicityPatterns(patternsDir)
    }

    // Load maximum inference pattern combinations parameter from file (default is 1)
    val maxCombinations = StringUtils.getInt(props, "maxCombinations", 1)


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

    // Step 5B: Automatically generate an IML runscript that will import that folder of IML patterns and enumerate them.
    val scriptStr = generateRunScript(IMLInputPath)

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
    val analysisStr = analysis(filteredQuestionsIn, inferencePatterns, tablestore, patterns, maxCombinations)
    println (analysisStr)

    // Output analysis
    val pw = new PrintWriter("analysis-explanationregenerationlowoverlap-q" + filteredQuestionsIn.length + "-maxCombinations" + maxCombinations + ".txt")
    pw.println(analysisStr)
    pw.close()

  }



}


// Storage class for pattern UUIDs
class InfPatUUIDs(val name:String, val uuids:Set[String], val rating:String) {

}

// Storage class
class CompletionCombinationEvaluationStr(val completions:Array[PatternMatchInfPat], val rowsTotal:Set[String], val rowsIntersection:Set[String], val numExtra:Int) {

  def getIntersectionSize():Int = rowsIntersection.size

  def getPercentage(qExplUUIDs:Set[String]):Double = {
    if (qExplUUIDs.size > 0) {
      return rowsIntersection.size.toDouble / qExplUUIDs.size.toDouble
    } else {
      0
    }
  }

}
