package inferenceengineexperiments

import data.question.{ExamQuestionParserDynamic, ExplanationRow, MCExplQuestion}
import data.tablerankings.ExternalQuestionRowRatings.loadRatingsFromSpreadsheet
import data.tablerankings.{ExternalQuestionRowRatings, ExternalTableRowRankings}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import data.question.ExamQuestionUtils.{convertToExplQuestions, filterQuestionsByFlags}
import explanationgraph.TableStore
//import explanationregeneration.RowEval
import inferenceengine.iml.parser.IMLParser
import inferenceengine.iml.runtime.{IMLReader, Interpreter}
import inferenceengine.struct.InferencePattern
import inferenceengine.util.LemmatizerSubstitutions
import inferenceengineexperiments.ExplanationRegenerationCeilingCalculator.{analysisWithExternalRankFiltering, erccLexicon, explanationToStr, makeProgressBarString}
import util.Histogram

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.control.Breaks.{break, breakable}

object ExplanationRegenerationExternalRankingPerformanceSummary {


  def analysisWithExternalRankFiltering(questions:Array[MCExplQuestion], pathExternalRankings:String, externalRankingMethod:String, tablestore:TableStore, topN:Int, manualRatings:Map[String, ExternalQuestionRowRatings]): String = {
    //val roleFilter = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    val os = new StringBuilder

    val roleFilter = Array("CENTRAL", "GROUNDING")

    var averageAccuracy:Double = 0.0
    var averageExtra:Double = 0.0
    var meanAvgPrecision:Double = 0.0

    var avgManualRatings = new Counter[String]
    var numManualRatingQ:Double = 0.0

    // For each question
    val qIdxs = Range(0, questions.length).par
    val numOfThreads = 1 // 14
    var numCompleted: Int = 0

    var errors = new ArrayBuffer[String]


    qIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numOfThreads))
    //for (i <- 0 until questions.length) {
    for (qIdx <- qIdxs) {
      val osThread = new StringBuilder

      val question = questions(qIdx)
      val qid = question.question.questionID
      val qExplUUIDs = question.getExplanationUUIDs(roleFilter).map(uuid => erccLexicon.add(uuid))

      // Progress bar
      println("numCompleted: " + numCompleted + " / " + questions.length)
      println(makeProgressBarString(currentProgress = numCompleted, maxProgress = questions.length, length = 70))

      println("Question " + qIdx + ": ")
      println(question.toString(tablestore))

      osThread.append("Question " + qIdx + ": \n")
      osThread.append(question.toString(tablestore) + "\n")


      // Load BERT rankings
      var rankings: ExternalTableRowRankings = null
      if (externalRankingMethod.toLowerCase() == "bert") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadBertRankings(filename, question.question.questionID)
        println("BERT Rankings: ")
        println(rankings.toString())
      } else if (externalRankingMethod.toLowerCase() == "tfidf") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadTfIdfRankings(filename, question.question.questionID)
        println("TF.IDF Rankings: ")
        println(rankings.toString())
      } else if (externalRankingMethod.toLowerCase() == "roberta-oyvind") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadRobertaRankingsOyvind(filename, question.question.questionID)
        println("ROBERTA (Oyvind) Rankings: ")
        println(rankings.toString())
      } else if (externalRankingMethod.toLowerCase() == "roberta-oyvind-peter") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadRobertaRankingsOyvindPeter(filename, question.question.questionID)
        println("ROBERTA (Oyvind-Peter) Rankings: ")
        println(rankings.toString())

      } else {
        throw new RuntimeException(" * analysisWithExternalRankFiltering(): ERROR: Unknown external ranking method specified (" + externalRankingMethod + ").")
      }

      // Baseline
      val (baselineAccuracy, baselineExtra, avgPrecision, baselineStr, ratingAvgQ) = getRankingBaselineWithManualRatings(qid, question, qExplUUIDs, rankings, topN, tablestore, manualRatings)
      println (baselineStr)
      println ("")

      osThread.append (baselineStr + "\n\n")

      // Synchronize so only one thread can access the following assignment block at a time
      synchronized {
        averageAccuracy += baselineAccuracy
        averageExtra += baselineExtra
        meanAvgPrecision += avgPrecision

        if (ratingAvgQ.size > 0) {
          for (key <- ratingAvgQ.keySet) {
            avgManualRatings.incrementCount(key, ratingAvgQ.getCount(key))
          }
          numManualRatingQ += 1
        }

        os.append( osThread )

        if (rankings.size <= 1) {
          errors.append("ERROR: Could not load external rankings for " + qid)
          println (errors.last)
        }


        numCompleted += 1
      }

    }


    // Average accuracy measures
    averageAccuracy = averageAccuracy / questions.length.toDouble
    averageExtra = averageExtra / questions.length.toDouble
    meanAvgPrecision = meanAvgPrecision / questions.length.toDouble


    // Average manual ratings
    for (key <- avgManualRatings.keySet) {
      val avg = avgManualRatings.getCount(key) / numManualRatingQ
      avgManualRatings.setCount(key, avg)
    }


    // Summary string

    os.append("\n\n\n")
    os.append("--------------------------------------------" + "\n")
    os.append("Summary Performance" + "\n")
    os.append("\n")
    os.append("Average Accuracy (@" + topN + "): " + averageAccuracy + "\n")
    os.append("Average Extra (@" + topN + "): " + averageExtra + "\n")
    os.append("MAP: " + meanAvgPrecision.formatted("%3.5f") + "\n")
    os.append("Number of questions in evaluation: " + questions.length + "\n")

    os.append("\n")
    os.append("Manual Ratings: " + "\n")
    val keysSorted = avgManualRatings.keySet.toArray.sorted
    for (key <- keysSorted) {
      os.append("  Average rank of rows rated " + key + ":\t")
      os.append(avgManualRatings.getCount(key.toString))
      os.append("\n")
    }
    os.append("Number of questions with manual ratings: " + numManualRatingQ + "\n")

    os.append("\n")
    os.append("Number of errors: " + errors.length + "\n")
    for (i <- 0 until errors.length) {
      os.append(i + ": " + errors(i) + "\n")
    }


    // Return
    os.toString
  }


  /*
   * Ranking baseline
   */

  // Returns (accuracy, extraRows)
  def getRankingBaselineWithManualRatings(qid:String, question:MCExplQuestion, goldUUIDs:Set[Int], rankings:ExternalTableRowRankings, topN:Int, tablestore:TableStore, manualRatings:Map[String, ExternalQuestionRowRatings]):(Double, Double, Double, String, Counter[String]) = {
    val rankingLength = math.min(topN, rankings.scoresRanked.length)

    val topNRankedUUIDs = rankings.scoresRanked.slice(0, rankingLength)
    val uuidsInRanks = new ArrayBuffer[String]()
    for (rankedUUID <- topNRankedUUIDs) uuidsInRanks.append(rankedUUID._1)

    var numCorrect:Double = 0
    for (goldUUID <- goldUUIDs) {
      val uuidStr = erccLexicon.get(goldUUID)
      if (uuidsInRanks.contains(uuidStr)) {
        numCorrect += 1
      }
    }

    // Score
    var accuracy = 0.0
    if (goldUUIDs.size > 0) {
      accuracy = numCorrect / goldUUIDs.size.toDouble
    }
    val extraRows = rankingLength - numCorrect

    // Score (manual ratings)
    val MAX_RATING = 10
    val ratingAvgs = new Counter[String]

    if (manualRatings.contains(qid)) {
      if (manualRatings(qid).fullyRated()) {          // Do not include questions where there are no manual labels
        val ratingCounts = new Counter[String]

        val ratings = manualRatings.get(qid).get
        for (rankIdx <- 0 until rankings.scoresRanked.length) {
          val rank = rankIdx + 1
          val uuid = rankings.scoresRanked(rankIdx)._1
          val manualRatingOpt = ratings.getRating(uuid)

          if (manualRatingOpt.isDefined) {
            val manualRating = manualRatingOpt.get

            println ("rating: " + manualRating + "\t rank: " + rank + "\t uuid: " + uuid)

            ratingAvgs.incrementCount(manualRating.toString, inc = rank)
            ratingCounts.incrementCount(manualRating.toString, inc = 1.0) // += 1
          }
        }

        // Calculate the average
        println ("Averaging")
        for (key <- ratingAvgs.keySet) {
          println ("key: " + key)
          println ("sum: " + ratingAvgs.getCount(key))
          println ("count: " + ratingCounts.getCount(key))

          val avg = ratingAvgs.getCount(key) / ratingCounts.getCount(key)
          ratingAvgs.setCount(key, avg)
        }
      }
    }

    val ap = averagePrecision(question, rankings)

    // Display
    val os = new StringBuilder

    os.append("Ranking Baseline: \n")
    os.append( explanationToStr(goldUUIDs, uuidsInRanks.toArray, tablestore, manualRatings.get(qid)) )
    os.append("Baseline Accuracy: " + accuracy.formatted("%3.3f") + "\n")
    os.append("Baseline Extra Rows: " + extraRows.formatted("%3.3f") + "\n")
    os.append("MAP: " + ap.formatted("%3.5f") + "\n")
    os.append("Manual Ratings: " + ratingAvgs.sorted.toString())


    // Return
    (accuracy, extraRows, ap, os.toString(), ratingAvgs)
  }


  /*
   * Scoring: Average precision
   */

  def averagePrecision(question:MCExplQuestion, rankings:ExternalTableRowRankings):Double = {
    val ranks = ranksOfCorrectRowsHelper(question.expl.toArray, rankings)
    val AP = calculateAPFromRanks(ranks)
    // Return
    AP
  }


  // Calculate average precision from a set of ranks
  def calculateAPFromRanks(ranksIn:Array[Int]):Double = {
    var sumPrecisions:Double = 0.0
    val numRanks = ranksIn.length

    // Case: Empty ranks list
    if (numRanks == 0) return 0.0

    // Case: Non-empty ranks list
    for (i <- 0 until numRanks) {
      val numCorrectAtRank = i + 1
      val precision:Double = numCorrectAtRank.toDouble / ranksIn(i).toDouble
      sumPrecisions += precision
    }

    // Return average precision
    sumPrecisions / numRanks.toDouble
  }

  /*
  def ranksOfCorrectRows():Array[Int] = {
    ranksOfCorrectRowsHelper(question.expl.toArray, rowEvals.toArray)
  }
   */

  // ranks (starting from 1)
  def ranksOfCorrectRowsHelper(toFindIn:Array[ExplanationRow], rankings:ExternalTableRowRankings):Array[Int] = {
    val out = new ArrayBuffer[Int]
    out.insertAll(0, offsetsOfCorrectRowsHelper(toFindIn, rankings))
    for (i <- 0 until out.length) {
      out(i) += 1
    }
    // Return
    out.toArray
  }

  // offsets (0 indexed)
  def offsetsOfCorrectRowsHelper(toFindIn:Array[ExplanationRow], rankings:ExternalTableRowRankings):Array[Int] = {
    val toFind = new ArrayBuffer[ExplanationRow]    // List of UIDs
    toFind.insertAll(0, toFindIn )             // Add list of UIDs for gold explanation from question
    val ranks = new ArrayBuffer[Int]

    for (i <- 0 until rankings.scoresRanked.length) {
      // Step 1: Determine if the rowEval at the current rank is part of the correct explanation, or not
      breakable {
        for (j <- 0 until toFind.length) {
          if (rankings.scoresRanked(i)._1 == toFind(j).uid) {
            ranks.append(i)
            toFind.remove(j)
            break()
          }
        }
      }

      // Check for early stopping condition
      if (toFind.length == 0) {
        return ranks.toArray
      }
    }

    // Edge case: Otherwise
    if (toFind.length > 0) {
      val missingUIDs = new ArrayBuffer[String]
      for (row <- toFind) {
        missingUIDs.append(row.uid)
      }
      println ("* WARNING: averagePrecision: toFind length is greater than zero (" + toFind.length +"). This likely indicates that a UID was not found. Ranks for this set will be incorrect. (UIDs = " + missingUIDs.mkString(", ") + ")" )
      for (i <- 0 until toFind.length) {
        ranks.append(-1)
      }
    }
    // Return
    ranks.toArray
  }



  /*
   * Supporting functions
   */

  // Display a ranked list, highlighting gold rows
  def explanationToStr(goldUUIDs:Set[Int], uuidList:Array[String], tablestore:TableStore, manualRatings:Option[ExternalQuestionRowRatings]):String = {
    val os = new StringBuilder

    for (i <- 0 until uuidList.length) {
      val uuid = uuidList(i)
      val row = tablestore.getRowByUID(uuid)


      os.append(i + ": \t")

      var manualRating = ""
      if (manualRatings.isDefined) {
        val rating = manualRatings.get.getRating(uuid)
        if (rating.isDefined) {
          manualRating = rating.get.toString
        }
      }
      if (row.tableName.startsWith("SYNONYMY")) {
        manualRating = "SYN"
      }

      os.append( manualRating.formatted("%5s") )

      if (goldUUIDs.contains(erccLexicon.add(uuidList(i)))) {
        os.append(" * ")    // Mark gold rows
      } else {
        os.append("   ")    // Non-gold row
      }
      os.append(uuid + "\t")
      val rowText = row.toStringText()
      os.append(rowText)

      os.append("\n")
    }

    // Return
    os.toString()
  }


  def main(args:Array[String]): Unit = {
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
    var filteredQuestionsIn = filterQuestionsByFlags(explQuestionsIn, Array("SUCCESS", "READY"))
    println("Loaded " + filteredQuestionsIn.size + " training questions after filtering. ")

/*
    // DEBUG: Further filter questions
    val qFiltered = new ArrayBuffer[MCExplQuestion]
    for (q <- filteredQuestionsIn) {
      if (!q.question.questionID.contains("_ENUM") || q.question.questionID.contains("_ENUM0")) {
        qFiltered.append(q)
      }
    }
    filteredQuestionsIn = qFiltered.toArray
*/


    // Step 3F: Load additional experiment parameters
    //val maxCombinations = StringUtils.getInt(props, "maxCombinations", 1)
    //val topNPerPattern = StringUtils.getInt(props, "topNPerPattern", 50)
    //val rankCutoff = StringUtils.getInt(props, "rankCutoff", -1)

    val pathExternalRankings = props.getProperty("pathExternalRankings", "")    // "explregen/bert/"
    val externalRankingMethod = props.getProperty("externalRankingMethod", "")                    // e.g. bert, tfidf


    // Load manual ratings
    val data = loadRatingsFromSpreadsheet("explregen/ExplanationRegeneration-ManualWorldtreeRatings-Apr24-2020-devonly.tsv")


    val topN = 20
    var summaryStr = analysisWithExternalRankFiltering(filteredQuestionsIn, pathExternalRankings, externalRankingMethod, tablestore, topN, data)

    summaryStr += "\n\n"
    summaryStr += "filenameQuestionsIn: " + filenameQuestionsIn + "\n"
    summaryStr += "pathExternalRankings: " + pathExternalRankings + "\n"
    summaryStr += "externalRankingMethod: " + externalRankingMethod + "\n"
    summaryStr += "topN: " + topN + "\n"



    println (summaryStr)
  }

}
