package explanationgraph

import edu.arizona.sista.struct.Lexicon
import util.TaggedLemmaHelper

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by user on 7/9/17.
  */

// 'twoLineHeader' specifies whether the header to each table has one line (old format) or two lines (new format for IML)
class TableStore(filename:String, twoLineHeader:Boolean = false, inTableSubdir:Boolean = true) extends Serializable {
  val tables = new ArrayBuffer[Table]
  val UIDtoTableLUT = mutable.Map[String, Int]().withDefaultValue(-1)

  // Lexicon
  val lexicon = new Lexicon[String]
  val isContentLemma = mutable.Map[Int, Boolean]().withDefaultValue(true)     // assume unknown words are content lemmas (e.g. nouns/verbs).

  /*
   * Constructor
   */
  loadTableStore(filename, twoLineHeader, inTableSubdir)
  //loadTableStoreParallel(filename, twoLineHeader, inTableSubdir)


  /*
   * Finding rows
   */
  def getRowTableByUID(uid:String):Int = {
    UIDtoTableLUT(uid)
  }

  def getRowByUID(uid:String):TableRow = {
    val tableIdx = getRowTableByUID(uid)
    if (tableIdx == -1) {
      // ERROR
      println ("ERROR: Could not find UID: " + uid + " (returning empty table row)")
      return new TableRow(new Table("", this), Array.empty[String], Array.empty[Array[Array[Int]]], Array.empty[Array[Array[Int]]], Array.empty[Array[Array[Int]]], Array.empty[Array[Array[Int]]], Array.empty[Array[Array[Int]]], Array.empty[Array[Array[Int]]])     // Note: Not ideal -- creates a new empty table reference for each UID that isn't found.
    }
    val table = tables(tableIdx)
    table.getRowByUID(uid)
  }


  /*
   * Keeping track of where rows with a given UUID are, for fast lookup/access
   */
  def addUIDToTableLUT(tableName:String, uuid:String):Boolean = {
    val tableIdx = findTableIdxByName(tableName)
    if (tableIdx.isDefined) {
      UIDtoTableLUT(uuid) = tableIdx.get
      return true
    } else {
      // Unable to find table name
      return false
    }
  }

  def removeUIDinTableLUT(uuid:String) {
    UIDtoTableLUT -= uuid
  }


  /*
   * Finding tables
   */
  def findTableIdxByName(name:String):Option[Int] = {
    for (i <- 0 until tables.size) {
      if (tables(i).name.toLowerCase == name.toLowerCase) {
        return Some(i)
      }
    }

    // Default: table could not be found
    None
  }

  def getTableByName(name:String):Option[Table] = {
    val idx = findTableIdxByName(name)
    if (idx.isEmpty) return None
    // Return
    Some(tables(idx.get))
  }

  def getTableNames():Array[String] = {
    val out = new ArrayBuffer[String]

    for (table <- tables) {
      out.append(table.name)
    }

    // Return
    out.toArray
  }

  /*
   * Using the Lexicon
   */
  def addAllToLexicon(word:String, lemma:String, tag:String) = {
    // Check if content tag
    val hasContentTag = TaggedLemmaHelper.hasContentTag(tag)
    val tWord = TaggedLemmaHelper.mkTLemma(word, tag)
    val tLemma = TaggedLemmaHelper.mkTLemma(lemma, tag)

    // Add to Lexicon
    val wordIdx = lexicon.add(word)
    val lemmaIdx = lexicon.add(lemma)
    val tWordIdx = lexicon.add(tWord)
    val tLemmaIdx = lexicon.add(tLemma)

    // If the word has a content tag, then add this information to our content tag look-up table
    if (hasContentTag) {
      isContentLemma(wordIdx) = true
      isContentLemma(lemmaIdx) = true
      isContentLemma(tWordIdx) = true
      isContentLemma(tLemmaIdx) = true
    } else {
      isContentLemma(wordIdx) = false
      isContentLemma(lemmaIdx) = false
      isContentLemma(tWordIdx) = false
      isContentLemma(tLemmaIdx) = false
    }

  }


  /*
   * Summary statistics
   */
  def numRows:Int = {
    var numTableRows:Int = 0
    for (table <- tables) {
      numTableRows += table.numRows()
    }
    // Return
    numTableRows
  }

  /*
   * Removing temporary rows
   */
  def removeTemporaryRows(UUIDPrefix:String = "TEMPGEN-"):Int = {
    var numRemoved:Int = 0
    for (table <- tables) {
      numRemoved += table.removeTemporaryRows(UUIDPrefix)
    }
    // Return
    numRemoved
  }

  def removeRow(uuid:String):Boolean = {
    val tableIdx = getRowTableByUID(uuid)
    if (tableIdx < 0) {
      return false
    }
    val table = tables(tableIdx)
    
    table.removeRow(uuid)
  }


  /*
   * Loading the tablestore
   */

  // Load a set of tables from a text file specifying the filenames of each table, one per line.
  def loadTableStore(filename:String, twoLineHeader:Boolean = false, inTableSubdir:Boolean = true): Unit = {
    println (" * loadTableStore: Started... (filename index = " + filename + ")")
    var tableRelativePath:String = ""
    if (inTableSubdir) tableRelativePath = filename.substring(0, filename.lastIndexOf("/") + 1) + "tables/"

    var numRows:Int = 0
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      val filenameTable = tableRelativePath + line
      if (addTable(filenameTable)) {
        numRows += tables.last.numRows
      }
    }

    println (" * loadTableStore: Complete. (" + tables.size + " tables loaded, containing a total of " + numRows + " rows)")
  }

  /*
  // Load a set of tables from a text file specifying the filenames of each table, one per line.
  // Note: This freezes about 1 time in 10 -- something is clearly not threadsafe, so do not use.
  def loadTableStoreParallel(filename:String, twoLineHeader:Boolean = false, inTableSubdir:Boolean = true): Unit = {
    println (" * loadTableStore: Started... (filename index = " + filename + ")")
    var tableRelativePath:String = ""
    if (inTableSubdir) tableRelativePath = filename.substring(0, filename.lastIndexOf("/") + 1) + "tables/"

    var numRows:Int = 0
    val lines = io.Source.fromFile(filename, "UTF-8").getLines().toArray
    val tablesTemp = Array.fill[Option[Table]](lines.length)(None)
    val fileIdxs = (0 until lines.length).par
    val numThreads = 4
    fileIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numThreads))
    for (fileIdx <- fileIdxs) {
      val filenameTable = tableRelativePath + lines(fileIdx)
      val table = new Table(filenameTable, this, twoLineHeader)
      tablesTemp(fileIdx) = Some(table)
    }

    for (table <- tablesTemp) {
      if (table.isDefined) {
        if (table.get.valid) {
          if (addTable(table.get)) {
            numRows += tables.last.numRows
          }
        }
      }
    }

    println (" * loadTableStore: Complete. (" + tables.size + " tables loaded, containing a total of " + numRows + " rows)")
  }
  */

  // Add a single table to the tablestore.  If the table is invalid, it will not be added.
  def addTable(filename:String):Boolean = {
    val table = new Table(filename, this, twoLineHeader)
    addTable(table)
  }

  // Add a single table to the tablestore.  If the table is invalid, it will not be added.
  def addTable(table:Table):Boolean = {
    if (table.valid) {
      tables.append(table)

      // Display table information
      println("\t" + tables.last.toString)

      // Add UIDs
      val tableIdx = tables.size - 1
      val UIDsInTable = table.getAllTableUIDs()
      for (i <- 0 until UIDsInTable.size) {
        val uid = UIDsInTable(i)
        UIDtoTableLUT(uid) = tableIdx
      }

      return true
    } else {
      println ("Table not valid: " + filename)
    }

    // Return
    false
  }



}


object TableStore {
  // Example usage
  val MAX_UNIQUE_TABLE_ROWS = 20000

  def main(args: Array[String]): Unit = {
    val tablestore = new TableStore("annotation/expl-tablestore-export-2017-07-09-160303/tableindex.txt")

    println ( tablestore.tables( tablestore.UIDtoTableLUT("a5c9-d7a4-8421-bb2e") ).name )

    println ( tablestore.getRowByUID("a5c9-d7a4-8421-bb2e") )

  }


}
