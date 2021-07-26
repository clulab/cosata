package inferenceengine.struct

import explanationgraph.{TableRow, TableStore}
import inferenceengine.iml.constraint.VariableList

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A list of constraints for table rows, that form a pattern that will match only limited table rows.
  * Created by peter on 6/22/18.
  */
class TableRowPattern(
  // A name for this pattern
  val name:String,

  // A list of names of table(s) that may satisfy the criterion for this row.
  // In the normal case, this will likely be only a single table.
  val tableNames:Array[String],

  // OR, if not tableNames, a list of static UUIDs that could fill this slot.
  val staticUUIDs:Array[String],

  // A list of column/value pairs that define specific values a row must take on in a given column to meet the criterion for this pattern
  //val columnValue:Array[ColumnValue]) {
  val cellPatterns:Array[CellPattern],

  // A boolean signifying whether this row exist, or is optional, in a given inference pattern.
  val isOptional:Boolean = false) {




  def hasStaticUUIDs:Boolean = {
    if (staticUUIDs.isEmpty) return false
    // Return
    true
  }

  def hasSingleStaticUUID:Boolean = {
    if (staticUUIDs.length == 1) return true
    // Return
    false
  }

  def isAllLexical(optionIdx:Int):Boolean = {
    for (cellPattern <- cellPatterns) {
      if (!cellPattern.isAllLexical(optionIdx)) return false
    }
    // Return
    true
  }

  // Get a list of TableRows matching the constriants in this pattern.
  def getTableRowsMatchingConstraints(tablestore:TableStore, varList:VariableList):Array[PatternMatchRowFlex] = {
    val out = new ArrayBuffer[PatternMatchRowFlex]

    // Step 1: Error checking
    if ((!tableNames.isEmpty) && (!staticUUIDs.isEmpty)) {
      throw new RuntimeException("ERROR: Both table name and static UUID references present for TableRowPattern -- only one should be present.")
    }

    // Step 2: Populate list of rows matching constraints
    if (!tableNames.isEmpty) {
      // Table name(s)
      for (tableName <- tableNames) {
        val table = tablestore.getTableByName(tableName)
        if (table.isEmpty) {
          throw new RuntimeException("ERROR: Could not find table with name '" + tableName + "'.")
        }

        out.insertAll(out.size, CellPattern.getAllRowsMatchingConstraint(table.get, cellPatterns, staticUUIDs = Array.empty[String], varList))
      }
    } else if (!staticUUIDs.isEmpty) {
      // Static UUIDs

      val firstRow = tablestore.getRowByUID(staticUUIDs(0))   // getAllRowsMatchingConstraint() requires a reference to 'table'. Here, we get it by checking the 'table' reference in the first static UUID.
      val table = firstRow.table

      out.insertAll(out.size, CellPattern.getAllRowsMatchingConstraint(table, cellPatterns, staticUUIDs=staticUUIDs, varList))

    } else {
      // Error
      throw new RuntimeException("ERROR: Neither table or static UUID references present for TableRowPattern.")
    }

    // Return
    out.toArray
  }


  // Find a list of constraints that are shared between this row and an input row.
  def findSharedVarConstraints(in:TableRowPattern):Array[(String, Array[String], Array[String])] = {    // (variableName, colName in this row, colName in that row)
    // Note, this looks across ALL OR Options.

    // Find variables in this pattern
    var variablesThis = mutable.Set[String]()
    for (cp <- this.cellPatterns) {
      for (optionIdxThis <- 0 until cp.getNumOptions()) {
        variablesThis = variablesThis ++ cp.getAllVariables(optionIdxThis)
      }
    }

    // Find variables in input/query pattern
    var variablesThat = mutable.Set[String]()
    for (cp <- in.cellPatterns) {
      for (optionIdxThat <- 0 until cp.getNumOptions()) {
        variablesThat = variablesThat ++ cp.getAllVariables(optionIdxThat)
      }
    }

    // Find intersection of two lists
    val shared = variablesThis.intersect(variablesThat)

    val out = new ArrayBuffer[(String, Array[String], Array[String])]()
    for (varName <- shared) {
      val peThis = new ArrayBuffer[String]
      for (cp <- this.cellPatterns) {
        for (optionIdxThis <- 0 until cp.getNumOptions()) {
          val elem = cp.getVariableNameElem(varName, optionIdxThis)
          if (elem.isDefined) peThis.append(cp.columnName)
        }
      }

      val peThat = new ArrayBuffer[String]
      for (cp <- in.cellPatterns) {
        for (optionIdxThat <- 0 until cp.getNumOptions()) {
          val elem = cp.getVariableNameElem(varName, optionIdxThat)
          if (elem.isDefined) peThat.append(cp.columnName)
        }
      }

      out.append( (varName, peThis.toArray, peThat.toArray) )
    }

    // Return
    out.toArray
  }



  /*
   * Find variables
   */
  def getVariablesInPattern():Set[String] = {
    // The assumption here is that all OR options for a given cell will have the same variables mentioned -- i.e. there won't be variables that only appear in some options.
    val out = mutable.Set[String]()

    for (cellPattern <- cellPatterns) {
      for (optionIdx <- 0 until cellPattern.getNumOptions()) {
        out ++= cellPattern.getAllVariables(optionIdx)
      }
    }

    // Return
    out.toSet
  }


  /*
   * Export to HTML (for debugging)
   */
  def getHTMLString(tablestore:TableStore, varList:VariableList, prefix:String = ""):String = {
    val os = new StringBuilder

    val flexMatches = getTableRowsMatchingConstraints(tablestore, varList)

    // Button label
    var buttonColor = "w3-light-grey"
    if (flexMatches.length > 0) {
      buttonColor = "w3-blue"
    }

    val buttonName = prefix + name + "infpatterns"
    os.append("<button onclick=\"accordion('" + buttonName + "')\" class=\"w3-button w3-block " + buttonColor + " w3-left-align\"> ")
    os.append(name + " (" + flexMatches.length + " matches). ")
    if (staticUUIDs.length > 0) os.append("[STATIC]")
    os.append(" </button> \n")

    // Accordion content
    os.append("<div id=\"" + buttonName + "\" class=\"w3-container w3-hide\"> \n")
    os.append("<blockquote> \n")

    // Row Pattern
    os.append( this.toString().replaceAll("\\n", "<br>") )

    // Matches
    for (i <- 0 until flexMatches.size) {
      os.append("<h2>Match " + i + "</h2><br>")

      //os.append( matches(i).toString(tablestore.lexicon).replaceAll("\\n", "<br>"))

      // TODO: This is still rough and not very usable -- improve this visualization
      os.append( flexMatches(i).row.toStringSentWithUID() + "<br>\n")
      for (optionIdx <- 0 until flexMatches(i).cellMatches.length) {
        for (m <- flexMatches(i).cellMatches(optionIdx)) {
          os.append("optionIdx: " + optionIdx + " ")
          os.append(m.toString(tablestore.lexicon) + " <br>\n")
        }
      }

    }

    os.append("</blockquote> \n")
    os.append("</div>\n")

    // Return
    os.toString()
  }

  /*
   * String methods
   */

  override def toString():String = {
    val os = new StringBuilder

    os.append("TableNames: " + tableNames.mkString(", ") + "\n")
    os.append("Cell Pattern Constraints: \n")
    for (i <- 0 until cellPatterns.length) {
      os.append(i + ": \t" + cellPatterns(i) + "\n")
    }
    os.append(" isOptional: " + isOptional)

    os.toString()
  }
}




object TableRowPattern {

}

