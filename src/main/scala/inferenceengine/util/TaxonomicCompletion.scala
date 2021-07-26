package inferenceengine.util

import com.sun.rowset.internal.Row
import edu.arizona.sista.struct.Lexicon
import explanationgraph.{Table, TableRow, TableStore}
import inferenceengine.iml.runtime.Interpreter.{GENERATEROW_UUIDPREFIX_PRESISTENT, GENERATEROW_UUIDPREFIX_TEMPORARY}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Completes taxonomic links in the tablestore
  * Created by peter on 4/9/2019
  */


object TaxonomicCompletion {
  val MODE_GENERATE_ROWS_USING_WORDS        =     1
  val MODE_GENERATE_ROWS_USING_LEMMAS       =     2

  val possibleFillMap = Map("[FILL] is" -> "is",
                            "[FILL] a kind of" -> "a kind of",
                            "[FILL] is a kind of" -> "is a kind of"
                            )

  def completeLinks(tablestore:TableStore, mode:Int): Unit = {
    println ("TaxonomicCompletion: Started... ")

    val MAX_ITER:Int = 50

    val table = tablestore.getTableByName("KINDOF").get
    val hypernymColIdx = table.getColumnIdx("HYPERNYM").get
    val hyponymColIdx = table.getColumnIdx("HYPONYM").get

    val alreadyCompleted = mutable.Map[Int, mutable.Set[String]]()

    val startTime = System.nanoTime()

    var addCountTotal:Int = -1
    var addCount:Int = -1
    var numIter:Int = 0

    while ((addCount != 0) && (numIter < MAX_ITER)) {
      // Reset the number of rows added in this iteration
      addCount = 0

      // In parallel: Assemble a list of candidate rows to add, as well as a list of row pairs already checked
      val numTableRows = table.numRows()
      val rowsToAdd = new Array[ArrayBuffer[TableRow]](numTableRows)
      val setsToAdd = new Array[mutable.Set[String]](numTableRows)
      for (i <- (0 until table.numRows()).par) {
        val (rowsToAdd_, setToAdd_) = mkRowsToAdd(table, i, hyponymColIdx, hypernymColIdx, alreadyCompleted, mode, tablestore)
        rowsToAdd(i) = rowsToAdd_
        setsToAdd(i) = setToAdd_
      }

      // Serial: Re-assemble the set of rows that have already been checked
      for (i <- 0 until numTableRows) {
        alreadyCompleted(i) = setsToAdd(i)
      }

      // Serial: Add candidate rows
      for (rowList <- rowsToAdd) {
        for (candidateRow <- rowList) {
          val (isDuplicate, duplicateUID) = table.isDuplicateRow(candidateRow)
          //println ("isDuplicate: " + isDuplicate)
          if (!isDuplicate) {
            // Unique row
            //println("TaxonomicCompletion: Generated Row: " + genRow.toString + "  (" + i + ": " + candidateRow.uid + ")  " + mkStringFromLexicon(yTerms, tablestore.lexicon))
            table.addRow(candidateRow)
            addCount += 1
            addCountTotal += 1
          }
        }
      }

      // Debug
      println ("Added " + addCount + " rows this iteration. ")
      numIter += 1
    }

    println ("Added " + addCountTotal + " total rows. ")

    println ("Taxonomic table now has " + table.numRows() + " rows.")
    val endTime = System.nanoTime()
    val deltaTimeMSec = (endTime - startTime) / 1000000
    println ("TaxonomicCompletion: Total time: " + deltaTimeMSec + " mSec")

  }

  // Helper function: this is intended to be called in parallel
  // TODO: Function across cell alternatives instead of just alt=0
  def mkRowsToAdd(table:Table, rowIdx:Int, hyponymColIdx:Int, hypernymColIdx:Int, alreadyCompleted:mutable.Map[Int, mutable.Set[String]], mode:Int, tablestore:TableStore):(ArrayBuffer[TableRow], mutable.Set[String]) = {
    val out = new ArrayBuffer[TableRow]
    var alreadyCompletedSet = alreadyCompleted.getOrElse(rowIdx, mutable.Set[String]())

    // [FILL] columns, depending on which are populated in this KINDOF table
    val fillMap = mutable.Map[String, String]()
    for (key <- possibleFillMap.keySet) {
      val fillColIdx = table.getColumnIdx(key)
      if (fillColIdx.isDefined) fillMap += (key -> possibleFillMap(key))
    }

    // Get row
    val row = table.getRowByIdx(rowIdx)

    // Gather X terms for new row generation (if applicable)
    val xTermsWords = row.getCellWordsAlt(hyponymColIdx, 0)

    // Gather X and Y terms in current row, for matching
    val xTerms = row.getCellLemmasAlt(hyponymColIdx, 0, onlyContentTags = true)

    val yTermsAlts = new ArrayBuffer[Array[Int]]()
    for (altIdx <- 0 until row.getCellNumAlternatives(hypernymColIdx)) {
      yTermsAlts.append(row.getCellLemmasAlt(hypernymColIdx, altIdx, onlyContentTags = true))
    }

    for (yTerms <- yTermsAlts) {
      // Find a shortlist of candidate rows to check
      val candidateRows = findCandidateRows(yTerms, table, hyponymColIdx)

      // Check each candidate row for a match.  If a match is found, generate a new row.
      for (candidateRow <- candidateRows) {

        // Check to see if we've already compared these rows before -- if we have, skip them to save time.
        //## val alreadyCompletedSet = alreadyCompleted.getOrElse(i, mutable.Set[String]())
        if (!alreadyCompletedSet.contains(candidateRow.uid)) {

          // Check for a match between yTerms and this row's hypernym column
          val candidateLemmas = candidateRow.getCellWordsAlt(hyponymColIdx, 0).toList
          if (candidateLemmas == yTerms.toList) {
            // Match
            // Gather Z terms
            for (altIdx <- 0 until candidateRow.getCellNumAlternatives(hypernymColIdx)) {
              val zTermsWords = candidateRow.getCellWordsAlt(hypernymColIdx, altIdx)
              val zTerms = candidateRow.getCellLemmasAlt(hypernymColIdx, alternative = altIdx, onlyContentTags = true)

              //val uuidPrefix = GENERATEROW_UUIDPREFIX_PRESISTENT
              val uuidPrefix = GENERATEROW_UUIDPREFIX_TEMPORARY

              // Create map specifying which columns are populated with specific data
              var cellValueMap = mutable.Map[String, String]()
              cellValueMap ++= fillMap

              if (mode == MODE_GENERATE_ROWS_USING_WORDS) {
                cellValueMap += ("HYPONYM" -> mkStringFromLexicon(xTermsWords, tablestore.lexicon), "HYPERNYM" -> mkStringFromLexicon(zTermsWords, tablestore.lexicon))
              } else if (mode == MODE_GENERATE_ROWS_USING_LEMMAS) {
                cellValueMap += ("HYPONYM" -> mkStringFromLexicon(xTerms, tablestore.lexicon), "HYPERNYM" -> mkStringFromLexicon(zTerms, tablestore.lexicon))
              } else {
                throw new RuntimeException("ERROR: Mode (" + mode + ") not recognized.")
              }

              // Generate new row
              val genRow = TableRow.mkTableRowGenerated(table, cellValueMap.toMap, uuidPrefix)
              if (genRow.isDefined) {

                out.append(genRow.get)
                /*
            // Serialize this
            val (isDuplicate, duplicateUID) = table.isDuplicateRow(genRow.get)
            //println ("isDuplicate: " + isDuplicate)
            if (!isDuplicate) {
              // Unique row
              //println("TaxonomicCompletion: Generated Row: " + genRow.toString + "  (" + i + ": " + candidateRow.uid + ")  " + mkStringFromLexicon(yTerms, tablestore.lexicon))
              table.addRow(genRow.get)
              //## addCount += 1
            }
             */
              }
            }

          }

          // Note that we've compared these rows
          //## alreadyCompleted(i) = alreadyCompletedSet ++ mutable.Set(candidateRow.uid)
          alreadyCompletedSet = alreadyCompletedSet ++ mutable.Set(candidateRow.uid)
        }
      }
    }

    // Return
    (out, alreadyCompletedSet)
  }


  def findCandidateRows(query:Array[Int], table:Table, colIdx:Int):Array[TableRow] = {
    // Create a list of row references for the table rows that have been found
    val rowIndicies = findCandidateRowsIndicies(query, table, colIdx).toArray
    val out = new Array[TableRow](rowIndicies.size)

    for (i <- 0 until rowIndicies.size) {
      out(i) = table.getRowByIdx(rowIndicies(i))
    }

    // Return
    out
  }

  def findCandidateRowsIndicies(query:Array[Int], table:Table, colIdx:Int):Set[Int] = {
    // Assemble a list of indices with all of the query terms
    var rowIndicies = Set[Int]()
    for (i <- 0 until query.length) {
      if (i == 0) {
        // First set
        rowIndicies = table.findRowsWithLexiconID(query(i), colIdx).toSet
      } else {
        // After each set, include only cases where all words are found
        rowIndicies = rowIndicies.intersect( table.findRowsWithLexiconID(query(i), colIdx).toSet )
      }
    }
    // Return
    rowIndicies
  }


  def mkStringFromLexicon(in:Array[Int], lexicon:Lexicon[String]):String = {
    val os = new StringBuilder

    for (lexiconIdx <- in) {
      os.append( lexicon.get(lexiconIdx) + " ")
    }

    // Return
    os.toString().trim()
  }

}
