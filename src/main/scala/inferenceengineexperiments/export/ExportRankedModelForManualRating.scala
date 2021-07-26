package inferenceengineexperiments.export

import java.io.PrintWriter

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import data.tablerankings.ExternalTableRowRankings
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.utils.StringUtils
import data.question.ExamQuestionUtils.{convertToExplQuestions, filterQuestionsByFlags}
import explanationgraph.{LookupLemmatizer, TableRow, TableStore}
import inferenceengine.util.LemmatizerSubstitutions
import util.TaggedLemmaHelper

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object ExportRankedModelForManualRating {
  val lookupLemmatizer = LookupLemmatizer


  def getQuestionTokens(question:MCExplQuestion):Set[String] = {
    val qTokens = mutable.Set[String]()

    // Question text
    for (sent <- question.question.annotation.sentences) {
      for (i <- 0 until sent.words.length) {
        val word = sent.words(i)
        val lemma = sent.lemmas.get(i)
        val tag = sent.tags.get(i)
        if (TaggedLemmaHelper.hasContentTag(tag)) {
          qTokens.add(word)
          qTokens.add(lemma)
          qTokens.add( lookupLemmatizer.getLemma(word) )
          qTokens.add( lookupLemmatizer.getLemma(lemma) )
        }
      }
    }

    // Answer text
    for (sent <- question.question.choices(question.question.correctAnswer).annotation.sentences) {
      for (i <- 0 until sent.words.length) {
        val word = sent.words(i)
        val lemma = sent.lemmas.get(i)
        val tag = sent.tags.get(i)
        if (TaggedLemmaHelper.hasContentTag(tag)) {
          qTokens.add(word)
          qTokens.add(lemma)
          qTokens.add( lookupLemmatizer.getLemma(word) )
          qTokens.add( lookupLemmatizer.getLemma(lemma) )
        }
      }
    }

    // Return
    qTokens.toSet
  }

  def getRowTokens(row:TableRow):Set[String] = {
    val rowTokens = mutable.Set[String]()
    val rowText = row.toStringText().trim()
    val words = rowText.split(" ")

    for (word <- words) {
      val lemma = lookupLemmatizer.getLemma(word)
      rowTokens.add(word)
      rowTokens.add(lemma)
    }

    return rowTokens.toSet
  }


  def rowToStrHighlight(question:MCExplQuestion, qTokens:Set[String], row:TableRow):String = {
    val rowText = row.toStringText().trim()
    val words = rowText.split(" ")

    var highlightOn:Boolean = false

    val os = new StringBuilder

    for (word <- words) {
      val lemma = lookupLemmatizer.getLemma(word)

      if (qTokens.contains(word) || qTokens.contains(lemma)) {
        if (!highlightOn) {
          os.append(" *")
        } else {
          os.append(" ")
        }
        highlightOn = true
      } else {
        if (highlightOn) {
          os.append("* ")
        } else {
          os.append(" ")
        }
        highlightOn = false
      }

      os.append(word)
    }

    if (highlightOn) {
      os.append("*")
    }


    // Return
    os.toString()
  }


  def findDistractorRow(qTokens:Set[String], goldUUIDs:Set[String], rankings:ExternalTableRowRankings, tablestore:TableStore):Option[(TableRow, Int)] = {
    val shortlist = new ArrayBuffer[(TableRow, Int)]()

    var startFrom:Int = 0
    val endAt:Int = rankings.scoresRanked.length
    if (endAt > 200) startFrom = 100

    // Populate shortlist
    while (shortlist.length < math.min(500, endAt-startFrom)) {
      val randIdx = Random.nextInt(endAt-startFrom) + startFrom
      val uuid = rankings.scoresRanked(randIdx)._1
      if (!goldUUIDs.contains(uuid)) {
        val row = tablestore.getRowByUID(uuid)
        val rowTokens = getRowTokens(row)
        val overlapCount = qTokens.intersect(rowTokens).size
        shortlist.append((row, overlapCount))
      }
    }

    if (shortlist.length == 0) {
      return None
    }

    // Pick top shortlist contender
    val sorted = shortlist.sortBy(-_._2)

    val bestDistractor = sorted(0)._1
    val rank = rankings.getRank(bestDistractor.uid).getOrElse(-1)

    return Some((bestDistractor, rank))
  }


  def ranksToRatingStr(qIdx:Int, question:MCExplQuestion, tablestore:TableStore, pathExternalRankings:String, rankingMethod:String, topN:Int = 20):String = {
    val os = new StringBuilder

    // Get list of gold explanation UUIDs
    val goldUUIDs = question.getExplanationUUIDs()

    // Load BERT rankings for this question
    val filename = pathExternalRankings + question.question.questionID + ".tsv"
    var rankings:ExternalTableRowRankings = null

    if (rankingMethod == "bert") {
      rankings = ExternalTableRowRankings.loadBertRankings(filename, question.question.questionID)
    } else if (rankingMethod == "roberta-oyvind") {
      //val filenameOyvindRoberta = pathExternalRankings + "/roberta_cl_wtrank-try1-eval-dev-all.tsv"
      rankings = ExternalTableRowRankings.loadRobertaRankingsOyvind(filename, question.question.questionID)
    } else {
      throw new RuntimeException("ERROR: Unknown ranking method (" + rankingMethod + ")")
    }

    //println("BERT Rankings: ")
    //println(rankings.toString())

    os.append(qIdx + "\t" + question.question.topic.mkString(", ") + "\n")
    os.append(question.question.questionID + "\t" + question.question.text + "\n")
    os.append("\t")
    for (i <- 0 until question.question.choices.length) {
      os.append("[")
      if (i == question.question.correctAnswer) os.append("*")
      os.append("]")
      os.append(question.question.choices(i).text + "   ")
    }
    os.append("\n")


    val qTokens = getQuestionTokens(question)
    val uuidsInList = mutable.Set[String]()

    // Make string with top N
    for (i <- 0 until math.min(rankings.scoresRanked.length, topN)) {
      val uuid = rankings.scoresRanked(i)._1
      val score = rankings.scoresRanked(i)._2
      //val rowText = tablestore.getRowByUID(uuid).toStringText()
      val row = tablestore.getRowByUID(uuid)
      val rowTable = row.tableName
      val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(uuid))

      if (rowTable.toUpperCase != "SYNONYMY") {
        var initialRanking: Int = 0
        if (goldUUIDs.contains(uuid)) initialRanking = 4

        os.append(i + "\t" + initialRanking + "\t" + rowText + "\t" + uuid + "\t" + score + "\n")
      } else {
        //os.append("filtered")
      }

      uuidsInList.add(uuid)
    }

    val toSort = new ArrayBuffer[(String, Int)]()
    // Also add the top explanation rows that are missing from the top-ranked N
    for (goldUUID <- goldUUIDs) {
      if (!uuidsInList.contains(goldUUID)) {
        val row = tablestore.getRowByUID(goldUUID)
        val rowTable = row.tableName
        val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(goldUUID))

        val rank = rankings.getRank(goldUUID).getOrElse(-1)
        val score = rankings.getScore(goldUUID).getOrElse(0.0)

        if (rowTable.toUpperCase != "SYNONYMY") {
          var initialRanking: Int = 4
          val str = (rank + "\t" + initialRanking + "\t" + rowText + "\t" + goldUUID + "\t" + score + "\n")
          toSort.append( (str, rank) )
        }
      }
    }
    val sorted = toSort.sortBy(_._2)
    for (line <- sorted) {
      os.append( line._1 )
    }

    // Also find a distractor
    val dist = findDistractorRow(qTokens, goldUUIDs, rankings, tablestore)
    if (dist.isDefined) {
      val (distractorRow, distractorRank) = dist.get
      val rowTextDistractor = rowToStrHighlight(question, qTokens, distractorRow)
      os.append(distractorRank + "\t" + "-1" + "\t" + rowTextDistractor + "\t" + distractorRow.uid + "\t" + "0.0" + "\n")
    }

    os.append("Accuracy@20: " + accuracy(question, pathExternalRankings, rankingMethod, topN))

    // Return
    os.toString()

  }

  def ranksToRatingStrMultiple(qIdx:Int, question:MCExplQuestion, tablestore:TableStore, pathExternalRankings1:String, rankingMethod1:String, pathExternalRankings2:String, rankingMethod2:String, topN:Int = 20):(String, Double, Double, Double, Double, Double) = {
    val os = new StringBuilder

    // Get list of gold explanation UUIDs
    val goldUUIDs = question.getExplanationUUIDs()

    // Ranking method 1: Load BERT rankings for this question
    val filename1 = pathExternalRankings1 + question.question.questionID + ".tsv"
    var rankings1:ExternalTableRowRankings = null

    if (rankingMethod1 == "bert") {
      rankings1 = ExternalTableRowRankings.loadBertRankings(filename1, question.question.questionID)
    } else if (rankingMethod1 == "roberta-oyvind") {
      //val filenameOyvindRoberta = pathExternalRankings + "/roberta_cl_wtrank-try1-eval-dev-all.tsv"
      rankings1 = ExternalTableRowRankings.loadRobertaRankingsOyvind(filename1, question.question.questionID)
    } else if (rankingMethod1 == "roberta-oyvind-peter") {
      rankings1 = ExternalTableRowRankings.loadRobertaRankingsOyvindPeter(filename1, question.question.questionID)
      //println("ROBERTA (Oyvind-Peter) Rankings: ")
      //println(rankings1.toString())
    } else {
      throw new RuntimeException("ERROR: Unknown ranking method (" + rankingMethod1 + ")")
    }

    // Ranking method 2: Load BERT rankings for this question
    val filename2 = pathExternalRankings2 + question.question.questionID + ".tsv"
    var rankings2:ExternalTableRowRankings = null

    if (rankingMethod2 == "bert") {
      rankings2 = ExternalTableRowRankings.loadBertRankings(filename2, question.question.questionID)
    } else if (rankingMethod2 == "roberta-oyvind") {
      //val filenameOyvindRoberta = pathExternalRankings + "/roberta_cl_wtrank-try1-eval-dev-all.tsv"
      rankings2 = ExternalTableRowRankings.loadRobertaRankingsOyvind(filename2, question.question.questionID)
    } else if (rankingMethod2 == "roberta-oyvind-peter") {
      rankings2 = ExternalTableRowRankings.loadRobertaRankingsOyvindPeter(filename2, question.question.questionID)
      //println("ROBERTA (Oyvind-Peter) Rankings: ")
      //println(rankings1.toString())
    } else {
      throw new RuntimeException("ERROR: Unknown ranking method (" + rankingMethod2 + ")")
    }



    //println("BERT Rankings: ")
    //println(rankings.toString())

    os.append("#" + qIdx + "\t" + question.question.topic.mkString(", ") + "\n")
    os.append("QID: " + question.question.questionID + "\t" + question.question.text + "\n")
    os.append("\t")
    for (i <- 0 until question.question.choices.length) {
      os.append("[")
      if (i == question.question.correctAnswer) os.append("*")
      os.append("]")
      os.append(question.question.choices(i).text + "   ")
    }
    os.append("\n")


    val qTokens = getQuestionTokens(question)
    val uuidsInList = mutable.Set[String]()

    // Get top N rows (method 1+2)
    val uuidsCombined = new Counter[String]

    // Method 1
    var idx:Int = 0
    var count:Int = 0
    while ((idx < rankings1.scoresRanked.length) && (count < topN)) {
      val uuid = rankings1.scoresRanked(idx)._1
      val score = rankings1.scoresRanked(idx)._2

      val row = tablestore.getRowByUID(uuid)
      val rowTable = row.tableName
      val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(uuid))

      if (rowTable.toUpperCase != "SYNONYMY") {     // Don't count rows from the SYNONYMY table
        uuidsCombined.incrementCount(uuid)
        count += 1
      }
      idx += 1
    }

    // Method 2
    idx = 0
    count = 0
    while ((idx < rankings2.scoresRanked.length) && (count < topN)) {
      val uuid = rankings2.scoresRanked(idx)._1
      val score = rankings2.scoresRanked(idx)._2

      val row = tablestore.getRowByUID(uuid)
      val rowTable = row.tableName
      val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(uuid))

      if (rowTable.toUpperCase != "SYNONYMY") {     // Don't count rows from the SYNONYMY table
        uuidsCombined.incrementCount(uuid)
        count += 1
      }
      idx += 1
    }

    val sortedUUIDs = uuidsCombined.sorted(descending = true).toArray
    val toSort = new ArrayBuffer[(String, Int)]()

    // Make string with top N
    for (i <- 0 until sortedUUIDs.length) {
      val uuid = sortedUUIDs(i)._1
      val score = sortedUUIDs(i)._2
      //val rowText = tablestore.getRowByUID(uuid).toStringText()
      val row = tablestore.getRowByUID(uuid)
      val rowTable = row.tableName
      val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(uuid))

      if (rowTable.toUpperCase != "SYNONYMY") {
        var initialRanking: Int = 0
        if (goldUUIDs.contains(uuid)) initialRanking = 4

        val str = ("0" + "\t" + rowText + "\t" + uuid + "\t" + score + "\t" + initialRanking + "\t" + i + "\t" + question.getRoleOfUUID(uuid))
        toSort.append( (str, i) )
      } else {
        //os.append("filtered")
      }

      uuidsInList.add(uuid)
    }

    // Also add the top explanation rows that are missing from the top-ranked N
    for (goldUUID <- goldUUIDs) {
      if (!uuidsInList.contains(goldUUID)) {
        val row = tablestore.getRowByUID(goldUUID)
        val rowTable = row.tableName
        val rowText = rowToStrHighlight(question, qTokens, tablestore.getRowByUID(goldUUID))

        val rank = rankings1.getRank(goldUUID).getOrElse(-1)
        val score = rankings1.getScore(goldUUID).getOrElse(0.0)

        if (rowTable.toUpperCase != "SYNONYMY") {
          var initialRanking: Int = 4
          val str = ("0" + "\t" + rowText + "\t" + goldUUID + "\t" + score + "\t" + initialRanking + "\t" + rank + "\t" + question.getRoleOfUUID(goldUUID))
          toSort.append( (str, rank) )
        }
      }
    }

    // Also find a distractor
    val dist = findDistractorRow(qTokens, goldUUIDs, rankings2, tablestore)
    var distractorStr:String = ""
    if (dist.isDefined) {
      val (distractorRow, distractorRank) = dist.get
      val rowTextDistractor = rowToStrHighlight(question, qTokens, distractorRow)
      distractorStr = "0" + "\t" + rowTextDistractor + "\t" + distractorRow.uid + "\t" + "0.0" + "\t" + "-1" + "\t" + distractorRank
      toSort.append( (distractorStr, Random.nextInt(toSort.size+1)) )
    }

    // Sort

    val sorted = toSort.sortBy(_._2)
    var distIdx = 0
    if (sorted.length > 2) distIdx = Random.nextInt(sorted.size-2)
    var countIdx:Int = 0
    for (i <- 0 until sorted.length) {
      os.append("\t" + sorted(i)._1 + "\t" + countIdx + "\n")
      countIdx += 1
      /*
      if (i == distIdx) {
        os.append("\t" + distractorStr + "\t" + countIdx + "\n")
        countIdx += 1
      }
       */
    }

    os.append("\t" + "\t" + "\t" + "notes" + "\t" + "0" + "\t" + "-2" + "\t" + "-2" + "\n")

    //os.append("Accuracy@" + topN + " (Method 1: " + rankingMethod1 + "): " + accuracy(question, pathExternalRankings1, rankingMethod1, topN))
    //os.append("Accuracy@" + topN + " (Method 2: " + rankingMethod2 + "): " + accuracy(question, pathExternalRankings2, rankingMethod2, topN))

    // Quick test -- check accuray of set where both methods said row was in the top-N
    val bothTopN = mutable.Set[String]()
    for (key <- uuidsCombined.keySet) {
      if (uuidsCombined.getCount(key) >= 2) {
        bothTopN.add(key)
      }
    }
    val accuracyCombined = accuracyOfSet(question, bothTopN.toSet)
    val sizeCombined = bothTopN.size.toDouble

    //## debug
    val goldUUIDs1 = question.getExplanationUUIDs(roleFilter = Array("CENTRAL", "GROUNDING"))
    os.append("bothTopN: " + bothTopN.mkString(", ") + "\n")
    os.append("goldUUIDs: " + goldUUIDs1.mkString(", "))
    os.append("accuracyCombined: " + accuracyCombined)

    println ("accuracyCombined: " + accuracyCombined)
    println ("sizeCombined: " + sizeCombined)

    // Accuracy of intersection
    val accuracyIntersection = accuracyOfSet(question, uuidsCombined.keySet.toSet)

    // Return
    (os.toString(), sorted.length.toDouble, uuidsCombined.size.toDouble, accuracyCombined, sizeCombined, accuracyIntersection)
  }


  def accuracy(question:MCExplQuestion, pathExternalRankings:String, rankingMethod:String, topN:Int = 20, roleFilter:Array[String] = Array("CENTRAL", "GROUNDING")):Double = {
    // Get list of gold explanation UUIDs
    val goldUUIDs = question.getExplanationUUIDs(roleFilter)

    // Load BERT rankings for this question
    val filename = pathExternalRankings + question.question.questionID + ".tsv"
    var rankings:ExternalTableRowRankings = null

    if (rankingMethod == "bert") {
      rankings = ExternalTableRowRankings.loadBertRankings(filename, question.question.questionID)
    } else if (rankingMethod == "roberta-oyvind") {
      //val filenameOyvindRoberta = pathExternalRankings + "/roberta_cl_wtrank-try1-eval-dev-all.tsv"
      rankings = ExternalTableRowRankings.loadRobertaRankingsOyvind(filename, question.question.questionID)
    } else if (rankingMethod == "roberta-oyvind-peter") {
      rankings = ExternalTableRowRankings.loadRobertaRankingsOyvindPeter(filename, question.question.questionID)
      //println("ROBERTA (Oyvind-Peter) Rankings: ")
      //println(rankings1.toString())
    } else {
      throw new RuntimeException("ERROR: Unknown ranking method (" + rankingMethod + ")")
    }


    // Make string with top N
    val inExpl = mutable.Set[String]()

    for (i <- 0 until math.min(rankings.scoresRanked.length, topN)) {
      val uuid = rankings.scoresRanked(i)._1
      val score = rankings.scoresRanked(i)._2

      inExpl.add(uuid)
    }

    // Calculate accuracy
    val intersection = goldUUIDs.intersect(inExpl)
    var accuracy = 0.0
    if (goldUUIDs.size > 0) {
      accuracy = intersection.size.toDouble / goldUUIDs.size.toDouble
    }

    // Return
    accuracy
  }

  def accuracyOfSet(question:MCExplQuestion, uuidsIn:Set[String], roleFilter:Array[String] = Array("CENTRAL", "GROUNDING")):Double = {
    // Get list of gold explanation UUIDs
    val goldUUIDs = question.getExplanationUUIDs(roleFilter)

    // Calculate accuracy
    val intersection = goldUUIDs.intersect(uuidsIn)
    var accuracy = 0.0
    if (goldUUIDs.size > 0) {
      accuracy = intersection.size.toDouble / goldUUIDs.size.toDouble
    }

    // Return
    accuracy
  }

  def main(args:Array[String]) = {

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

    println ("Shuffling... ")
    Random.setSeed(1L)
    val shuffled = Random.shuffle(filteredQuestionsIn.toList).sortBy(_.question.topic(0)).toArray
    filteredQuestionsIn = shuffled

    println("Loaded " + filteredQuestionsIn.size + " training questions after filtering. ")


    // Step 3F: Load additional experiment parameters
    val maxCombinations = StringUtils.getInt(props, "maxCombinations", 1)
    val topNPerPattern = StringUtils.getInt(props, "topNPerPattern", 50)
    val rankCutoff = StringUtils.getInt(props, "rankCutoff", -1)

    val pathExternalRankings1 = props.getProperty("pathExternalRankings1", "")    // "explregen/bert/"
    val externalRankingMethod1 = props.getProperty("externalRankingMethod1", "")                    // e.g. bert, tfidf
    val pathExternalRankings2 = props.getProperty("pathExternalRankings2", "")    // "explregen/bert/"
    val externalRankingMethod2 = props.getProperty("externalRankingMethod2", "")                    // e.g. bert, tfidf



    val topN = 8


    var accSum1:Double = 0.0
    var accSum2:Double = 0.0
    var avgRowsManualRate:Double = 0.0
    var avgRowsIntersection:Double = 0.0
    var lastFilenameIndex = 0

    var combinedAcc:Double = 0.0
    var avgRowsCombined:Double = 0.0

    var avgAccIntersection:Double = 0.0

    //val filenameOut = "manualratings_worldtree_all-" + lastFilenameIndex + ".tsv"
    //val filenameOut = "manualratings_worldtree_all-filtered1-" + externalRankingMethod1 + "-" + externalRankingMethod2 + "-traindevtest.tsv"
    val filenameOut = "manualratings_worldtree_all-filtered1-" + externalRankingMethod1 + "-" + externalRankingMethod2 + "-topN" + topN + "-devonly.tsv"
    var pw = new PrintWriter(filenameOut)


    for (i <- 0 until filteredQuestionsIn.length) {
      /*
      val filenameIndex = math.floor(i / 1000).toInt
      if (filenameIndex != lastFilenameIndex) {
        pw.close()

        lastFilenameIndex = filenameIndex
        val filenameOut = "manualratings_worldtree_all-" + lastFilenameIndex + ".tsv"
        pw = new PrintWriter(filenameOut)
      }
       */

      println (i)
      //val str = ranksToRatingStr(i, filteredQuestionsIn(i), tablestore, pathExternalRankings, externalRankingMethod, topN)
      val (str, numRowsManualRate, numRowsCombined, combinedAccuracy, combinedRowLength, accuracyInterection) = ranksToRatingStrMultiple(i, filteredQuestionsIn(i), tablestore, pathExternalRankings1, externalRankingMethod1, pathExternalRankings2, externalRankingMethod2, topN)
      accSum1 += accuracy(filteredQuestionsIn(i), pathExternalRankings1, externalRankingMethod1, topN)
      accSum2 += accuracy(filteredQuestionsIn(i), pathExternalRankings2, externalRankingMethod2, topN)
      avgRowsManualRate += numRowsManualRate
      avgRowsIntersection += numRowsCombined

      combinedAcc += combinedAccuracy
      avgRowsCombined += combinedRowLength

      avgAccIntersection += accuracyInterection

      //println (question.toString(tablestore))
      pw.println (str)

      pw.println("combinedAccuracy: " + combinedAccuracy)
      pw.println("avgRowsCombined: " + combinedRowLength)

      pw.println ("")
      pw.println ("---------------------------------------------------------------------------------")
      pw.println ("")
    }

    pw.close()

    accSum1 = accSum1 / filteredQuestionsIn.length.toDouble
    accSum2 = accSum2 / filteredQuestionsIn.length.toDouble
    println ("Average Accuracy @" + topN + " (Method 1: " + pathExternalRankings1 + ") : " + accSum1)
    println ("Average Accuracy @" + topN + " (Method 2: " + pathExternalRankings2 + ") : " + accSum2)
    pw.println ("Average Accuracy @" + topN + " (Method 1: " + pathExternalRankings1 + ") : " + accSum1)
    pw.println ("Average Accuracy @" + topN + " (Method 2: " + pathExternalRankings2 + ") : " + accSum2)

    avgRowsManualRate = avgRowsManualRate / filteredQuestionsIn.length.toDouble
    avgRowsIntersection = avgRowsIntersection / filteredQuestionsIn.length.toDouble
    avgAccIntersection = avgAccIntersection / filteredQuestionsIn.length.toDouble
    println ("Average number of rows (to manually rate, including missing gold rows): " + avgRowsManualRate.formatted("%3.1f"))
    println ("Average number of rows (intersection of top-N of both methods): " + avgRowsIntersection.formatted("%3.1f"))
    println ("Accuracy of intersection rows (" + avgRowsIntersection.formatted("%3.1f") + " rows): " + avgAccIntersection.formatted("%3.3f"))
    pw.println ("Average number of rows (to manually rate, including missing gold rows): " + avgRowsManualRate.formatted("%3.1f"))
    pw.println ("Average number of rows (intersection of top-N of both methods): " + avgRowsIntersection.formatted("%3.1f"))
    pw.println ("Accuracy of intersection rows (" + avgRowsIntersection.formatted("%3.1f") + " rows): " + avgAccIntersection.formatted("%3.3f"))

    println ("")
    pw.println ("")
    combinedAcc = combinedAcc / filteredQuestionsIn.length.toDouble
    avgRowsCombined = avgRowsCombined / filteredQuestionsIn.length.toDouble
    println ("Accuracy of combined method (both methods have rows in top-" + topN +"): " + combinedAcc.formatted("%3.3f"))
    println ("Average number of rows shared between both models in top-" + topN + "): " + avgRowsCombined.formatted("%3.3f"))
    pw.println ("Accuracy of combined method (both methods have rows in top-" + topN +"): " + combinedAcc.formatted("%3.3f"))
    pw.println ("Average number of rows shared between both models in top-" + topN + "): " + avgRowsCombined.formatted("%3.3f"))


  }

}
