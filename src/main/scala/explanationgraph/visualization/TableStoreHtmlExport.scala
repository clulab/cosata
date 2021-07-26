package explanationgraph.visualization

import java.io.PrintWriter

import explanationgraph.{Table, TableStore}
import TableStoreHtmlExport._
import util.LexiconUtil

import scala.collection.mutable.ArrayBuffer

/**
  * Created by user on 7/31/18.
  */
class TableStoreHtmlExport(tablestore:TableStore) {


  // Generate HTML for a single table
  def exportTable(table:Table, mode:Int):String = {
    val os = new StringBuilder
    val onlyContentTags = true

    // Start table
    os.append("<div style=\"overflow-x:auto;\">")
    os.append("<table class=\"log w3-small\" id=\"t01\">\n")

    // Header
    os.append("<tr class=\"log\">")
    for (columnName <- table.header) {
      os.append("<th class=\"log\">" + columnName + "</th>")
    }
    os.append("</tr>\n")

    // Rows
    for (i <- 0 until table.numRows()) {
      val row = table.getRowByIdx(i)

      os.append("<tr class=\"log\">")
      if (mode == MODE_RAW) {
        for (cell <- row.cells) {
          os.append("<td class=\"log\">" + cell + "</td>")
        }
      } else {
        for (colIdx <- 0 until row.cells.length) {
          val cellAltStrs = new ArrayBuffer[String]

          if ((table.getColumnRole(colIdx) == Table.ROLE_DATA) || (table.getColumnRole(colIdx) == Table.ROLE_FILL)) {
            // Data or fill column

            for (altCellIdx <- 0 until row.getCellNumAlternatives(colIdx)) {
              var cellContents = Array.empty[Int]
              if (mode == MODE_WORDS) {
                cellContents = row.getCellWordsAlt(colIdx, altCellIdx) // Note: changed variable matching to matching Lemmas instead of words
              } else if (mode == MODE_LEMMAS) {
                cellContents = row.getCellLemmasAlt(colIdx, altCellIdx, onlyContentTags = true)
              } else if (mode == MODE_TWORDS) {
                cellContents = row.getCellTWordsAlt(colIdx, altCellIdx)
              } else if (mode == MODE_TLEMMAS) {
                cellContents = row.getCellTLemmasAlt(colIdx, altCellIdx)
              }
              cellAltStrs.append(LexiconUtil.lexiconIdxsToStr(cellContents, table.lexicon))
            }

            var cellStr = ""
            if (row.getCellNumAlternatives(colIdx) > 1) {
              cellStr = "(" + cellAltStrs.mkString(" ; ") + ")"
            } else {
              cellStr = cellAltStrs.mkString("*") // This should never be more than a single element -- but just incase, added the ";" so it's easy to identify these issues.
            }

            os.append("<td class=\"log\">" + cellStr + "</td>")
          } else {
            // Metadata column
            os.append("<td class=\"log\">" + row.cells(colIdx) + "</td>")
          }
        }
      }


      os.append("</tr>\n")
    }

    // End table
    os.append("</table>")
    os.append("</div>")

    // Return
    os.toString()
  }


  // Generate accordian-HTML for a list of tables.  Also includes the accordian script.
  def exportTables(tableNames:Array[String], mode:Int):String = {
    val os = new StringBuilder

    for (tableName <- tableNames) {

      val table = tablestore.getTableByName(tableName)
      var numRows:Int = 0
      if (table.isDefined) numRows = table.get.numRows()

      os.append("<button onclick=\"accordion('" + tableName +"')\" class=\"w3-button w3-block w3-blue w3-left-align\"> ")
      os.append(tableName + " (" + numRows + " rows)")
      os.append(" </button> \n")


      if (table.isDefined) {

        os.append("<div id=\"" + tableName + "\" class=\"w3-container w3-hide\"> \n")
        os.append(exportTable(table.get, mode) + "\n")
        os.append("<br>\n")
        os.append("</div>\n")
      }

    }


    // Add the accordion script
    os.append("<script>\n")
    os.append("function accordion(id) { \n")
    os.append("  var x = document.getElementById(id);\n")
    os.append("  if (x.className.indexOf(\"w3-show\") == -1) { \n")
    os.append("    x.className += \" w3-show\"; \n")
    os.append("  } else { \n")
    os.append("    x.className = x.className.replace(\" w3-show\", \"\"); \n")
    os.append("  }\n")
    os.append("} \n")
    os.append("</script>\n")


    // Return
    os.toString()
  }


  // Generate HTML for the entire tablestore, including headers/formatting.
  def generateHTML(filename:String, mode:Int): Unit = {
    val os = new StringBuilder

    println (" * TableStoreHtmlExport.generateHTML(): started... (filename = " + filename + ")")

    os.append("<html> \n")
    os.append("<head> \n")
    os.append("<title> TableStore export </title> \n")
    os.append(" <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\"> \n")
    os.append("<style>")
    os.append("table.log, th.log, td.log {\n    border: 1px solid black;\n border-collapse: collapse;\n padding: 5px;\n}")
    os.append("table.log#t01 tr:nth-child(even) {\n    background-color: #eee;\n}\ntable.log#t01 tr:nth-child(odd) {\n    background-color: #fff;\n}\ntable.log#t01 th {\n   background-color: #cce4ff;\n}")
    os.append("</style>")
    os.append("</head> \n")
    os.append("<body> \n")

    os.append("Export mode: " + modeToStr(mode) )

    os.append(" <div class=\"w3-container\"> \n")


    // Generate HTML
    val tableNames = tablestore.getTableNames()
    println ("tableNames: " + tableNames.mkString(", "))
    os.append( exportTables(tableNames, mode) )


    os.append(" </div> \n")
    os.append("</body> \n")
    os.append("</html> \n")

    // Export
    val pw = new PrintWriter(filename)
    pw.println( os.toString() )
    pw.close()

    println (" * TableStoreHtmlExport.generateHTML(): export completed... ")

  }


}


object TableStoreHtmlExport {
  val MODE_RAW      =     0
  val MODE_WORDS    =     1
  val MODE_LEMMAS   =     2
  val MODE_TWORDS   =     3
  val MODE_TLEMMAS  =     4

  def modeToStr(mode:Int):String = {
    mode match {
      case MODE_RAW  =>     "MODE_RAW"
      case MODE_WORDS =>    "MODE_WORDS"
      case MODE_LEMMAS =>   "MODE_LEMMAS"
      case MODE_TWORDS =>   "MODE_TWORDS"
      case MODE_TLEMMAS =>  "MODE_TLEMMAS"
      case _ =>             "UNKNOWN MODE"
    }
  }
}