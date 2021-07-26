package inferenceengineexperiments

import java.io.PrintWriter

import data.question.{ExamQuestionParserDynamic, MCExplQuestion}
import data.tablerankings.ExternalTableRowRankings
import edu.arizona.sista.struct.{Counter, Lexicon}
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
import inferenceengineexperiments.ExplanationRegenerationLowOverlap.{mkTopCombinations, patternMatchToUUIDSet}
import util.Histogram

import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by peter on 1/22/19.
  */

object ExplanationRegenerationCeilingCalculator {
  val erccLexicon = new Lexicon[String]

  /*
   * Analyses
   */
  def analysis (questions:Array[MCExplQuestion], populatedPatterns:Array[InferencePattern]): Unit = {
    //val roleFilter = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    val roleFilter = Array("CENTRAL", "GROUNDING")

    val maxCombinations = 2
    val hists = new Array[Counter[Int]](maxCombinations)
    for (i <- 0 until hists.length) hists(i) = new Counter[Int]

    val histsPercent = new Array[Histogram](maxCombinations)
    for (i <- 0 until histsPercent.length) histsPercent(i) = new Histogram(name = i + " Pattern(s) Combined")

    var averageOverlaps = Array.fill[Double](maxCombinations)(0.0)
    var averageExtra = Array.fill[Double](maxCombinations)(0.0)

    // For each question
    val qIdxs = Range(0, questions.length).par
    val numOfThreads = 14
    qIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numOfThreads))
    //for (i <- 0 until questions.length) {
    for (qIdx <- qIdxs) {
      val question = questions(qIdx)
      val qExplUUIDs = question.getExplanationUUIDs(roleFilter).map( uuid => erccLexicon.add(uuid) )


      println ("Question " + qIdx + ": ")
      println (question.toString())

      val shortlist = getListOfCompletions(question, populatedPatterns)

      println ("")

      println ("Shortlist has " + shortlist.length + " completions. ")

      for (patternsToCombine <- 1 to maxCombinations) {
        //val combinations = mkCombinations(question, shortlist, numPatternsToCombine = patternsToCombine)
        val combinations = findTopCombination(question, shortlist, numPatternsToCombine = patternsToCombine, roleFilter)

        println("Combinations with " + patternsToCombine + " patterns: " + combinations.size)

        println ("Evaluating... ")

        var maxSize = 0
        var maxPercentOverlap = 0.0
        var maxExtra = 0
        if (combinations.length > 0) {
          maxSize = combinations(0).getIntersectionSize()
          maxPercentOverlap = combinations(0).getPercentage(qExplUUIDs)
          maxExtra = combinations(0).numExtra
        }

        // Synchronize so only one thread can access the following assignment block at a time
        synchronized {
          hists(patternsToCombine - 1).incrementCount(maxSize)
          histsPercent(patternsToCombine - 1).addData(maxPercentOverlap, tag = "")

          averageOverlaps(patternsToCombine - 1) += maxPercentOverlap
          averageExtra(patternsToCombine - 1) += maxExtra
        }

      }

      println ("")
      println ("------------------------------------------------------------------------------")
      println ("")


    }


    // Step 8: Inference pattern summary
    println ("")
    println ("Summary of Inference Patterns (and Number of Matches) in Analysis:")
    var sumPatternEnumerations:Int = 0
    for (i <- 0 until populatedPatterns.length) {
      val name = populatedPatterns(i).name
      val numPatternMatches = populatedPatterns(i).fullPatternMatches.length
      println (i + "\t" + numPatternMatches.formatted("%10s") + "\t" + name)
      sumPatternEnumerations += numPatternMatches
    }
    println ("")
    println ("Total pattern enumerations: " + sumPatternEnumerations)
    println ("")

    // Calculate averages
    for (i <- 0 until averageOverlaps.length) averageOverlaps(i) = averageOverlaps(i) / questions.length.toDouble
    for (i <- 0 until averageExtra.length) averageExtra(i) = averageExtra(i) / questions.length.toDouble


    println ("Histogram: ")
    // Header
    print("Overlap".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (i + " pattern(s)").formatted("%20s")
      print(str)
    }
    println ("")
    // Data
    for (j <- 0 until 15) {
      print(j.formatted("%20s"))
      for (i <- 1 to maxCombinations) {
        val count = hists(i-1).getCount(j)
        val str = count.formatted("%20s")
        print(str)
      }
      println ("")
    }
    println ("")


    // Average percentage overlaps
    print("AvgOverlap".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageOverlaps(i-1).formatted("%3.4f")).formatted("%20s")
      print(str)
    }
    println ("")
    println ("")

    // Average number of extra rows
    print("AvgExtra".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageExtra(i-1).formatted("%3.4f")).formatted("%20s")
      print(str)
    }
    println ("")
    println ("")

    // Percentage histograms
    for (i <- 1 to maxCombinations) {
      println(histsPercent(i-1).toString())
    }



    println ("roleFilter: " + roleFilter.mkString(", "))

  }


  def analysisWithExternalRankFiltering (questions:Array[MCExplQuestion], populatedPatterns:Array[InferencePattern], pathExternalRankings:String, externalRankingMethod:String, tablestore:TableStore, maxCombinations:Int = 1, topNPerPattern:Int = 50, rankCutoff:Int = -1): String = {
    //val roleFilter = Array("CENTRAL", "GROUNDING", "LEXGLUE")
    val os = new StringBuilder

    val roleFilter = Array("CENTRAL", "GROUNDING")

    val hists = new Array[Counter[Int]](maxCombinations)
    for (i <- 0 until hists.length) hists(i) = new Counter[Int]

    val histsPercent = new Array[Histogram](maxCombinations)
    for (i <- 0 until histsPercent.length) histsPercent(i) = new Histogram(name = i + " Pattern(s) Combined")

    var averageOverlaps = Array.fill[Double](maxCombinations)(0.0)
    var averageExtra = Array.fill[Double](maxCombinations)(0.0)

    var averageOverlapsBaseline = Array.fill[Double](maxCombinations)(0.0)
    var averageExtraBaseline = Array.fill[Double](maxCombinations)(0.0)

    var averageSize = Array.fill[Double](maxCombinations)(0.0)
    var averageSizeNonZero = Array.fill[Double](maxCombinations)(0.0)
    var averageSizeCountNonZero = Array.fill[Double](maxCombinations)(0.0)    // Counter to normalize averageSizeNonZero

    // For each question
    val qIdxs = Range(0, questions.length).par
    val numOfThreads = 1        // 14
    var numCompleted:Int = 0

    qIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numOfThreads))
    //for (i <- 0 until questions.length) {
    for (qIdx <- qIdxs) {
      val osThread = new StringBuilder

      val question = questions(qIdx)
      val qExplUUIDs = question.getExplanationUUIDs(roleFilter).map( uuid => erccLexicon.add(uuid) )

      // Progress bar
      println ("numCompleted: " + numCompleted + " / " + (questions.length * maxCombinations))
      println ( makeProgressBarString(currentProgress = numCompleted, maxProgress = (questions.length*maxCombinations), length = 70) )

      println ("Question " + qIdx + ": ")
      println (question.toString(tablestore))

      osThread.append("Question " + qIdx + ": \n")
      osThread.append(question.toString(tablestore) + "\n")


      // Load BERT rankings
      var rankings:ExternalTableRowRankings = null
      if (externalRankingMethod.toLowerCase() == "bert") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadBertRankings(filename, question.question.questionID)
        println("BERT Rankings: ")
        println(rankings.toString())
      } else if (externalRankingMethod.toLowerCase() == "tfidf") {
        val filename = pathExternalRankings + question.question.questionID + ".tsv"
        rankings = ExternalTableRowRankings.loadTfIdfRankings(filename, question.question.questionID)
        println("BERT Rankings: ")
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

      val (shortlist, osCompletions) = getFilteredListOfCompletions(question, populatedPatterns, rankings, topNPerPattern, rankCutoff)
      osThread.append(osCompletions + "\n")

      println ("")

      println ("Shortlist has " + shortlist.length + " completions after filtering by ranking scores. ")
      osThread.append ("Shortlist has " + shortlist.length + " completions after filtering by ranking scores. ")

      for (patternsToCombine <- 1 to maxCombinations) {
        //val combinations = mkCombinations(question, shortlist, numPatternsToCombine = patternsToCombine)
        val combinations = findTopCombinationWithRanks(question, shortlist, rankings, numPatternsToCombine = patternsToCombine, roleFilter)

        println("Combinations with " + patternsToCombine + " patterns: " + combinations.size)
        osThread.append("Combinations with " + patternsToCombine + " patterns: " + combinations.size + "\n")

        println ("Evaluating... ")

        var maxSize = 0
        var maxPercentOverlap = 0.0
        var maxExtra = 0
        var experimentalUUIDs = Set.empty[String]
        var maxScore = 0.0

        if (combinations.length > 0) {
          maxSize = combinations(0).getIntersectionSize()
          maxPercentOverlap = combinations(0).getPercentage(qExplUUIDs)
          maxExtra = combinations(0).numExtra
          experimentalUUIDs = combinations(0).getUUIDs()
          maxScore = combinations(0).score
        }

        // Display ceiling explanation
        println ("Ceiling Explanation: ")
        println ("Score: " + maxScore)
        println( explanationToStr(qExplUUIDs, experimentalUUIDs.toArray, tablestore) )
        println ("---")
        osThread.append ("Ceiling Explanation: \n")
        osThread.append ("Score: " + maxScore + "\n")
        osThread.append ( explanationToStr(qExplUUIDs, experimentalUUIDs.toArray, tablestore) + "\n")
        osThread.append ("--- \n")

        if (combinations.length > 0) {
          println(combinations(0).getString(tablestore))
          osThread.append(combinations(0).getString(tablestore) + "\n")
        }


        // Baseline
        val experimentalExplanationLength = experimentalUUIDs.size
        val (baselineAccuracy, baselineExtra, baselineStr) = getRankingBaseline(qExplUUIDs, rankings, topN = experimentalExplanationLength, tablestore)
        println (baselineStr)
        println ("")

        osThread.append (baselineStr + "\n\n")

        // Synchronize so only one thread can access the following assignment block at a time
        synchronized {
          hists(patternsToCombine - 1).incrementCount(maxSize)
          histsPercent(patternsToCombine - 1).addData(maxPercentOverlap, tag = "")

          averageOverlaps(patternsToCombine - 1) += maxPercentOverlap
          averageExtra(patternsToCombine - 1) += maxExtra

          averageOverlapsBaseline(patternsToCombine - 1) += baselineAccuracy
          averageExtraBaseline(patternsToCombine - 1) += baselineExtra

          averageSize(patternsToCombine - 1) += experimentalExplanationLength
          if (experimentalExplanationLength > 0) {
            averageSizeNonZero(patternsToCombine - 1) += experimentalExplanationLength
            averageSizeCountNonZero(patternsToCombine - 1) += 1
          }

          numCompleted += 1
        }

      }

      osThread.append ("\n")
      osThread.append ("------------------------------------------------------------------------------ \n")
      osThread.append ("\n")

      synchronized {
        os.append( osThread )
      }


    }


    // Step 8: Inference pattern summary
    os.append ("analysisWithBERTFiltering():" + "\n\n")

    os.append ("Summary of Inference Patterns (and Number of Matches) in Analysis:" + "\n")
    var sumPatternEnumerations:Int = 0
    for (i <- 0 until populatedPatterns.length) {
      val name = populatedPatterns(i).name
      val numPatternMatches = populatedPatterns(i).fullPatternMatches.length
      os.append (i + "\t" + numPatternMatches.formatted("%10s") + "\t" + name + "\n")
      sumPatternEnumerations += numPatternMatches
    }
    os.append ("\n")
    os.append ("Total pattern enumerations: " + sumPatternEnumerations + "\n")
    os.append ("\n")

    // Calculate averages
    for (i <- 0 until averageOverlaps.length) averageOverlaps(i) = averageOverlaps(i) / questions.length.toDouble
    for (i <- 0 until averageExtra.length) averageExtra(i) = averageExtra(i) / questions.length.toDouble

    for (i <- 0 until averageOverlapsBaseline.length) averageOverlapsBaseline(i) = averageOverlapsBaseline(i) / questions.length.toDouble
    for (i <- 0 until averageExtraBaseline.length) averageExtraBaseline(i) = averageExtraBaseline(i) / questions.length.toDouble

    for (i <- 0 until averageSize.length) averageSize(i) = averageSize(i) / questions.length.toDouble
    for (i <- 0 until averageSizeNonZero.length) averageSizeNonZero(i) = averageSizeNonZero(i) / averageSizeCountNonZero(i)

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
    os.append ("\n")

    os.append("AvgOverlapBaseline".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageOverlapsBaseline(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append ("\n")

    os.append ("\n")

    // Average number of extra rows
    os.append("AvgExtra".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageExtra(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append ("\n")

    os.append("AvgExtraBaseline".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageExtraBaseline(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append ("\n")

    os.append ("\n")

    // Average size of explanations
    os.append("AvgSize".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageSize(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append ("\n")

    os.append("AvgSizeNonZero".formatted("%20s"))
    for (i <- 1 to maxCombinations) {
      val str = (averageSizeNonZero(i-1).formatted("%3.4f")).formatted("%20s")
      os.append(str)
    }
    os.append ("\n")

    os.append ("\n")


    // Percentage histograms
    for (i <- 1 to maxCombinations) {
      os.append(histsPercent(i-1).toString())
    }



    os.append ("roleFilter: " + roleFilter.mkString(", ") + "\n")
    os.append ("maxCombinations: " + maxCombinations + "\n")
    os.append ("topNPerPattern: " + topNPerPattern + "\n")
    os.append ("rankCutoff: " + rankCutoff + "\n")

    os.append ("externalRankingMethod: " + externalRankingMethod + "\n")
    os.append ("pathExternalRankings: " + pathExternalRankings + "\n")

    // Return summary performance string
    os.toString
  }

  /*
  // Gives a complete list of possible combinations, but is memory intensive
  def mkCombinations(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[String], Set[String])], numPatternsToCombine:Int):Array[CompletionCombinationEvaluation] = {
    val out = new ArrayBuffer[CompletionCombinationEvaluation]

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
        out.append(new CompletionCombinationEvaluation(completionList, completionUUIDsSum.toSet, intersection.toSet))
        maxIntersectionSize = intersection.size
      }
    }

    // Return
    out.toArray
  }
  */

  def findTopCombination(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[Int], Set[Int])], numPatternsToCombine:Int, roleFilter:Array[String]):Array[CompletionCombinationEvaluation] = {
    println (" * findTopCombination(): Started... (numPatternsToCombine: " + numPatternsToCombine + ")")
    var out:Option[CompletionCombinationEvaluation] = None
    val MAX_SIZE = 10000

    val qExplUUIDs = question.getExplanationUUIDs(roleFilter).map( uuid => erccLexicon.add(uuid) )
    val qExplUUIDsSize = qExplUUIDs.size

    val maxValues = Array.fill[Int](numPatternsToCombine)(shortlist.size)
    val iter = new CombinationIterator(maxValues)

    var maxIntersectionSize:Int = 0
    var maxExtraSize:Int = 0

    var count:Long = 0
    while (iter.hasNext()) {
      val indices = iter.next()
      var completionUUIDsSum = mutable.Set[Int]()

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
        out = Some(new CompletionCombinationEvaluation(completionList, completionUUIDsSum.toSet, intersection.toSet, numExtra))
        maxIntersectionSize = intersection.size
        maxExtraSize = numExtra
      }

      count += 1
      if (count % 1000000 == 0) {
        val percent = ((count.toDouble / iter.size.toDouble)*100).formatted("%3.1f") + "%"
        print(percent + "  ")
      }
      if (count % 20000000 == 0) {
        println ("")
      }
    }

    // Return
    if (out.isDefined) {
      Array(out.get)
    } else {
      Array.empty[CompletionCombinationEvaluation]
    }
  }


  // Get a list of inference pattern completions that have non-zero overlap with the explanation for a given question
  def getListOfCompletions(question:MCExplQuestion, populatedPatterns:Array[InferencePattern]):Array[(PatternMatchInfPat, Set[Int], Set[Int])] = {
    val explUUIDs = question.getExplanationUUIDs().map( uuid => erccLexicon.add(uuid) )

    // For each inference pattern, check that pattern's completions for overlap with this explanation.
    // Add any completion with non-zero overlap to a shortlist.
    val patternsWithNonZeroOverlap = new ArrayBuffer[(PatternMatchInfPat, Set[Int], Set[Int])]()       // Shortlist
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
  def patternMatchToUUIDSet(in:PatternMatchInfPat):Set[Int] = {
    val out = mutable.Set[Int]()
    for (slot <- in.rowSlots) {
      if (slot.isDefined) {
        val uuidStr = slot.get.row.uid
        val uuidLexiconID = erccLexicon.add(uuidStr)
        out.add(uuidLexiconID)
      }
    }
    // Return
    out.toSet
  }

  /*
   * BERT-filtered rankings
   */

  // Get a list of inference pattern completions that have non-zero overlap with the explanation for a given question
  def getFilteredListOfCompletions(question:MCExplQuestion, populatedPatterns:Array[InferencePattern], rankings:ExternalTableRowRankings, topNPerPattern:Int = 50, rankCutoff:Int = -1):(Array[(PatternMatchInfPat, Set[Int], Set[Int])], String) = {
    val os = new StringBuilder

    val explUUIDs = question.getExplanationUUIDs().map( uuid => erccLexicon.add(uuid) )

    // For each inference pattern, check that pattern's completions for overlap with this explanation.
    // Add any completion with non-zero overlap to a shortlist.
    val patternsWithNonZeroOverlap = new ArrayBuffer[(PatternMatchInfPat, Set[Int], Set[Int])]()       // Shortlist
    for (i <- 0 until populatedPatterns.length) {
      val completions = populatedPatterns(i).fullPatternMatches

      val topNShortList = new ArrayBuffer[(PatternMatchInfPat, Set[Int], Set[Int], Double)]()       // Shortlist
      for (completion <- completions) {
        // Convert from pattern match completion storage class to a set of UUIDs representing that completion
        val uuids = patternMatchToUUIDSet(completion)
        // Check for overlap between completion and question explanation
        val intersection = explUUIDs.intersect(uuids)
        // If overlap, proceed
        if (intersection.size > 0) {
          // Calculate score for this pattern
          val score = getRankedPatternScore(uuids, rankings, rankCutoff)

          // Store in shortlist
          topNShortList.append( (completion, uuids, intersection, score) )
        }
      }

      // Sort top N shortlist by score, and trim to top N
      var sorted = topNShortList.sortBy(-_._4)
      if (sorted.length > topNPerPattern) {
        sorted = sorted.slice(0, topNPerPattern)
      }

      // Debug: Output top score of a given pattern, to ensure everything is working well
      if (sorted.length > 0) {
        println("TopScore: " + sorted(0)._4 + "\t" + populatedPatterns(i).name)
        os.append("TopScore: " + sorted(0)._4 + "\t" + populatedPatterns(i).name + "\n")
      }

      // Add to master pattern list
      // patternsWithNonZeroOverlap.insertAll(patternsWithNonZeroOverlap.length, sorted)
      // Add to master pattern list (strip scores -- note, discarded here to use existing infrastructure to calculate overlap, but can always be exported then filtered later if needed)
      for (j <- 0 until sorted.length) {
        val repack = (sorted(j)._1, sorted(j)._2, sorted(j)._3)
        patternsWithNonZeroOverlap.append(repack)
      }
    }

    // Return
    (patternsWithNonZeroOverlap.toArray, os.toString)
  }

  def getRankedPatternScore(uuids:Set[Int], rankings:ExternalTableRowRankings, rankCutoff:Int = -1): Double = {
    var scoreSum:Double = 0.0
    var cutoffScore:Double = 0.0

    if (rankCutoff >= 0) {
      // If enabled, get score at rank cutoff
      if (rankings.scoresRanked.length > rankCutoff) {
        cutoffScore = rankings.scoresRanked(rankCutoff)._2
      }
    }

    for (uuid <- uuids) {
      val uuidStr = erccLexicon.get(uuid)
      val score = rankings.getScore(uuidStr)

      if (score.isDefined) {
        var scoreToAdd:Double = score.get

        // Handle rank cutoff -- if the rank of the uuid exceeds a given value, then add the score of the last element at the cutoff
        if (rankCutoff >= 0) {
          val uuidRank = rankings.rankedUUIDs.getOrElse(uuidStr, Int.MinValue)
          if (uuidRank < rankCutoff) {
            scoreToAdd = rankCutoff
          }
        }

        // Add to score
        scoreSum += scoreToAdd
      }
    }

    //## println ("Score: " + scoreSum)

    // Return total score of this pattern
    scoreSum
  }

  def findTopCombinationWithRanks(question:MCExplQuestion, shortlist:Array[(PatternMatchInfPat, Set[Int], Set[Int])], rankings:ExternalTableRowRankings, numPatternsToCombine:Int, roleFilter:Array[String]):Array[CompletionCombinationEvaluation] = {
    println (" * findTopCombination(): Started... (numPatternsToCombine: " + numPatternsToCombine + ")")
    var out:Option[CompletionCombinationEvaluation] = None
    val MAX_SIZE = 10000

    val qExplUUIDs = question.getExplanationUUIDs(roleFilter).map( uuid => erccLexicon.add(uuid) )
    val qExplUUIDsSize = qExplUUIDs.size

    val maxValues = Array.fill[Int](numPatternsToCombine)(shortlist.size)
    val iter = new CombinationIterator(maxValues)

    var maxIntersectionSize:Int = 0
    var maxExtraSize:Int = 0
    var maxScore:Double = -Double.MaxValue

    var count:Long = 0
    while (iter.hasNext()) {
      val indices = iter.next()
      val indicesUnique = indices.toSet.toArray

      var completionUUIDsSum = mutable.Set[Int]()

      val completionList = new Array[PatternMatchInfPat](indicesUnique.length)
      for (i <- 0 until indicesUnique.length) {
        completionList(i) = shortlist(indicesUnique(i))._1    // PatternMatchInfPat only
        val completionUUIDs = patternMatchToUUIDSet(completionList(i))
        completionUUIDsSum = completionUUIDsSum.union(completionUUIDs)
      }

      val intersection = completionUUIDsSum.intersect(qExplUUIDs)
      val numExtra = completionUUIDsSum.diff(qExplUUIDs).size

      var rankScore:Double = 0.0
      for (uuid <- completionUUIDsSum) {
        val score = rankings.getScore( erccLexicon.get(uuid) )
        if (score.isDefined) {
          rankScore += score.get
        }
      }

      // TODO: Only store if the match has a high number of intersections, to save memory?

      // Store evaluation
      // **NOTE**: For this ceiling analysis, to save time/memory, only store those overlaps that are at least as large as the maximum overlap previously encountered for this question
      // ** This means the list returned will not be complete, so not useful for downstream tasks other than ceiling performance calculation. **

      //if ((intersection.size > maxIntersectionSize) || ((intersection.size == maxIntersectionSize) && (numExtra < maxExtraSize))) {     // Minimize length
      if ((intersection.size > maxIntersectionSize) || ((intersection.size == maxIntersectionSize) && (rankScore > maxScore))) {          // Maximize score from external rankings (e.g. BERT)
        out = Some(new CompletionCombinationEvaluation(completionList, completionUUIDsSum.toSet, intersection.toSet, numExtra, score = rankScore))
        maxIntersectionSize = intersection.size
        maxExtraSize = numExtra
        maxScore = rankScore
      }

      count += 1
      if (count % 1000000 == 0) {
        val percent = ((count.toDouble / iter.size.toDouble)*100).formatted("%3.1f") + "%"
        print(percent + "  ")
      }
      if (count % 20000000 == 0) {
        println ("")
      }
    }

    // Return
    if (out.isDefined) {
      Array(out.get)
    } else {
      Array.empty[CompletionCombinationEvaluation]
    }
  }

  /*
   * Ranking baseline
   */

  // Returns (accuracy, extraRows)
  def getRankingBaseline(goldUUIDs:Set[Int], rankings:ExternalTableRowRankings, topN:Int, tablestore:TableStore):(Double, Double, String) = {
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

    // Display
    val os = new StringBuilder

    os.append("Ranking Baseline: \n")
    os.append( explanationToStr(goldUUIDs, uuidsInRanks.toArray, tablestore) )
    os.append("Baseline Accuracy: " + accuracy.formatted("%3.3f") + "\n")
    os.append("Baseline Extra Rows: " + extraRows.formatted("%3.3f") + "\n")



    // Return
    (accuracy, extraRows, os.toString())
  }


  // Display a ranked list, highlighting gold rows
  def explanationToStr(goldUUIDs:Set[Int], uuidList:Array[String], tablestore:TableStore):String = {
    val os = new StringBuilder

    for (i <- 0 until uuidList.length) {
      val uuidStr = uuidList(i)
      os.append(i + ": \t")
      if (goldUUIDs.contains(erccLexicon.add(uuidList(i)))) {
        os.append(" * ")    // Mark gold rows
      } else {
        os.append("   ")    // Non-gold row
      }
      os.append(uuidStr + "\t")
      val rowText = tablestore.getRowByUID(uuidStr).toStringText()
      os.append(rowText)

      os.append("\n")
    }

    // Return
    os.toString()
  }


  /*
   * Generate IML runscript
   */

  // Generate an IML run script that will import an entire folder of IML patterns, enumerate them using the constraint satisfaction framework, and exit.
  def generateRunScript(folder:String, exportJSON:Boolean = true, maxMatches:Int):String = {
    val scriptCode = new ArrayBuffer[String]

    scriptCode.append("importFolder \"" + folder + "\"")
    scriptCode.append("")
    scriptCode.append("// Perform constraint satisfaction over the inference pattern and generate the HTML logfile output. ")

    scriptCode.append("setEnvironmentVariable(\"maxMatches\", \"" + maxMatches + "\")")   // Set the maximum number of matches in the enumeration

    scriptCode.append("executeAutoPatterns")      // For the automatically generated KINDOF additions

    scriptCode.append("populateInfPatMatches")    // Run patterns

    scriptCode.append("incrementState")
    scriptCode.append("exportInfPatHTML()")
    if (exportJSON) scriptCode.append("exportInfPatJSON()")
    scriptCode.append("exportTableStoreHTML()")
    scriptCode.append("exportStateSpaceHTML()")

    val scriptStr = scriptCode.mkString("\n")

    // Return
    scriptStr
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

  def makeProgressBarString(currentProgress:Double, maxProgress:Double, length:Int):String = {
    val os = new StringBuilder

    val proportion = currentProgress / maxProgress
    val lengthDone = math.floor(length.toDouble * proportion).toInt
    val lengthTodo = (length - lengthDone).toInt

    os.append("[")
    os.append("|" * lengthDone)
    os.append("-" * lengthTodo)
    os.append("] ")
    os.append((proportion*100).formatted("%3.1f") + "%")

    // Return
    os.toString()
  }



  /*
   * Main
   */

  def main(args:Array[String]) = {
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
    val filteredQuestionsIn = filterQuestionsByFlags(explQuestionsIn, Array("SUCCESS", "READY"))
    println("Loaded " + filteredQuestionsIn.size + " training questions after filtering. ")


    // Step 3F: Load additional experiment parameters
    val maxCombinations = StringUtils.getInt(props, "maxCombinations", 1)
    val topNPerPattern = StringUtils.getInt(props, "topNPerPattern", 50)
    val rankCutoff = StringUtils.getInt(props, "rankCutoff", -1)

    val pathExternalRankings = props.getProperty("pathExternalRankings", "")    // "explregen/bert/"
    val externalRankingMethod = props.getProperty("externalRankingMethod", "")                    // e.g. bert, tfidf


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

    // Load the maximum amount to enumerate any one inference pattern (or, set to a default value of 25000)
    val maxMatches = StringUtils.getInt(props, "maxMatches", 25000)

    // Step 5B: Automatically generate an IML runscript that will import that folder of IML patterns and enumerate them.
    val scriptStr = generateRunScript(IMLInputPath, exportJSON = false, maxMatches = maxMatches)

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
    //analysis(filteredQuestionsIn, inferencePatterns)

    var summaryStr = analysisWithExternalRankFiltering(filteredQuestionsIn, inferencePatterns, pathExternalRankings, externalRankingMethod, tablestore, maxCombinations, topNPerPattern, rankCutoff)

    // Add additional parameter reporting to summary string
    summaryStr += "maxMatches: " + maxMatches + "\n"
    summaryStr += "filenameQuestionsIn: " + filenameQuestionsIn + "\n"
    summaryStr += "imlInputPath: " + IMLInputPath + "\n"
    summaryStr += "tablestoreIndex: " + tablestoreIndex + "\n"

    // Add execution time to summary string
    val endTimeTotal = System.nanoTime()
    val deltaTimeTotal = (endTimeTotal - startTime) / 1000000000L
    summaryStr += "Execution time (total): " + deltaTimeTotal + " seconds" + "\n"


    // Output summary string
    println (summaryStr)

    val filenameSummaryStr = "out-analysisceiling-" + externalRankingMethod + "-q" + questionsIn.length + "-qf" + filteredQuestionsIn.length + "-infpats" + inferencePatterns.length + "-maxCombinations" + maxCombinations + "-topNPerPat" + topNPerPattern + "-rankCutoff" + rankCutoff + "-maxMatches" + maxMatches + ".txt"
    val pw = new PrintWriter(filenameSummaryStr)
    pw.println(summaryStr)
    pw.close()


  }



}


// Storage class
class CompletionCombinationEvaluation(val completions:Array[PatternMatchInfPat], val rowsTotal:Set[Int], val rowsIntersection:Set[Int], val numExtra:Int, val score:Double = 0.0) {

  def getIntersectionSize():Int = rowsIntersection.size

  def getPercentage(qExplUUIDs:Set[Int]):Double = {
    if (qExplUUIDs.size > 0) {
      return rowsIntersection.size.toDouble / qExplUUIDs.size.toDouble
    } else {
      0
    }
  }

  def getUUIDs():Set[String] = {
    val out = mutable.Set[String]()
    for (completion <- completions) {
      val (uuids, _) = completion.getPatternUUIDs()
      out ++= uuids
    }
    // Return
    out.toSet
  }

  def getString(tablestore:TableStore):String = {
    val os = new StringBuilder
    for (i <- 0 until completions.length) {
      val completion = completions(i)
      os.append("Pattern " + i + "\n")
      os.append( completion.toStringMinimalSummary() + "\n")
    }
    // Return
    os.toString()
  }

}
