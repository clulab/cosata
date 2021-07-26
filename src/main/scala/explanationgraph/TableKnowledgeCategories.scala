package explanationgraph

import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by user on 4/10/18.
  */
class TableKnowledgeCategories {

}

object TableKnowledgeCategories {
  val logger = LoggerFactory.getLogger(classOf[TableKnowledgeCategories])
  val FILENAME_DEFAULT_TABLEKNOWLEDGECATEGORIES = "annotation/expl-tablestore-export-2017-08-25-230344/tableKnowledgeCategories.tsv"

  val tableCategoryHashmap = mutable.Map[String, String]()
  val knowledgeCategories = mutable.Set[String]()
  loadTableKnowledgeCategories(FILENAME_DEFAULT_TABLEKNOWLEDGECATEGORIES)

  // Get the category of a table
  def getCategory(tableName:String):String = {
    val tableNameNormalized = tableName.toUpperCase
    if (!tableCategoryHashmap.contains(tableNameNormalized)) return "UNKNOWN"
    // Return
    tableCategoryHashmap(tableNameNormalized)
  }

  // Get a list of all tables in a given category
  def getTablesInCategory(category:String):Array[String] = {
    val out = new ArrayBuffer[String]
    for (key <- tableCategoryHashmap.keySet) {
      if (getCategory(key) == category) {
        out.append(key)
      }
    }
    out.toArray
  }

  // Get a list of all categories
  def getCategories():mutable.Set[String] = {
    knowledgeCategories
  }


  def loadTableKnowledgeCategories(filename:String) = {
    logger.info (" * Loading table knowledge categories (" + filename + ")...")

    for(line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      //println (line)
      val normalized = line.replaceAll("[\\s]+", "\t").trim()
      val split = line.toLowerCase.split("\t")
      val tableName = split(0).trim.toUpperCase
      val category = split(1).trim.toUpperCase

      tableCategoryHashmap += (tableName -> category)
      knowledgeCategories += category
    }

    logger.info (" * Table knowledge categories loaded.  (" + tableCategoryHashmap.size + " table -> category pairs)" )
  }


}
