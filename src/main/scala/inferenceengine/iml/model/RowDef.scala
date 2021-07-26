package inferenceengine.iml.model

import explanationgraph.TableStore
import inferenceengine.struct.TableRowPattern

/**
  * Created by peter on 7/9/18.
  */
case class RowDef(name:String, rowref:RowRef, isOptional:Boolean) extends StatementInf {

  def toTableRowPattern(tablestore:TableStore):TableRowPattern = {
    println ("toTableRowPattern: started... ")
    val cellPatterns = new Array[inferenceengine.struct.CellPattern](rowref.cellRefs.length)

    // Convert cellReferences into inferenceengine.struct.CellPatterns
    for (i <- 0 until rowref.cellRefs.length) {
      cellPatterns(i) = rowref.cellRefs(i).toCellPatterns(tablestore)
    }

    // Determine mode
    if (rowref.usesTable()) {
      val tableName = rowref.getTableName()
      return new TableRowPattern(name, tableNames = Array(tableName), staticUUIDs = Array.empty[String], cellPatterns, isOptional = isOptional)
    } else if (rowref.usesUUID()) {
      val uuid = rowref.getUUID()

      // Efficiency: Check to see if this static row has entirely lexical constraints.  If so, the lexical constraints can be assumed to be for human-readability, and removed for efficiency.
      var hasVariables:Boolean = false
      for (cell <- cellPatterns) {
        for (optionIdx <- 0 until cell.getNumOptions()) {
          if (!cell.isAllLexical(optionIdx)) hasVariables = true
        }
      }

      if (hasVariables) {
        return new TableRowPattern(name, tableNames = Array.empty[String], staticUUIDs = Array(uuid), cellPatterns, isOptional = isOptional)
      } else {
        // Static row with entirely lexical pattern -- remove lexical constraints
        return new TableRowPattern(name, tableNames = Array.empty[String], staticUUIDs = Array(uuid), cellPatterns = Array.empty[inferenceengine.struct.CellPattern], isOptional = isOptional)
      }
    }


    // We should never reach here -- throw exception if we have.
    throw new RuntimeException("toTableRowPattern: RowRef did not reference either Table or UUID.")
  }


  override def toString():String = {
    val os = new StringBuilder

    os.append("[")
    os.append("rowVariableName: " + name + "   ")
    os.append( rowref.toString )
    os.append("]")

    // Return
    os.toString()
  }
}

