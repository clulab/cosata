package inferenceengine.iml.model

import explanationgraph.TableStore
import inferenceengine.struct.{PatternElem, TableRowPattern}

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Positional

/**
  * Created by peter on 7/9/18.
  */

case class RowRef(table:Option[TableRef], uuid:Option[UUIDRef], cellRefs:List[CellRef]) extends Positional {

  def usesTable():Boolean = {
    if (table.isEmpty) return false
    true
  }

  def usesUUID():Boolean = {
    if (uuid.isEmpty) return false
    true
  }

  def getTableName():String = {
    table.get.name
  }

  def getUUID():String = {
    uuid.get.uuid
  }

  override def toString():String = {
    val os = new StringBuilder

    if (usesTable()) {
      os.append(table.get.toString() + ", ")
    }
    if (usesUUID()) {
      os.append(uuid.get.toString() + ", ")
    }
    os.append( cellRefs.mkString(", ") )

    // Return
    os.toString()
  }
}

// Alternate, using Exprs for cell content
case class RowRefExpr(table:Option[TableRef], uuid:Option[UUIDRef], cellRefs:List[CellRefExpr]) extends Positional {

  def usesTable():Boolean = {
    if (table.isEmpty) return false
    true
  }

  def usesUUID():Boolean = {
    if (uuid.isEmpty) return false
    true
  }

  def getTableName():String = {
    table.get.name
  }

  def getUUID():String = {
    uuid.get.uuid
  }

  override def toString():String = {
    val os = new StringBuilder

    if (usesTable()) {
      os.append(table.get.toString() + ", ")
    }
    if (usesUUID()) {
      os.append(uuid.get.toString() + ", ")
    }
    os.append( cellRefs.mkString(", ") )

    // Return
    os.toString()
  }
}



case class TableRef(name:String) extends Positional {

  override def toString():String = {
    val os = new StringBuilder

    os.append("TABLE: " + name)

    // Return
    os.toString()
  }

}

case class UUIDRef(uuid:String) extends Positional {

  override def toString():String = {
    val os = new StringBuilder

    os.append("UUID: " + uuid)

    // Return
    os.toString()
  }

}

case class CellRef(colName:String, cellPattern:List[CellPattern]) extends Positional {

  def toCellPatterns(tablestore:TableStore):inferenceengine.struct.CellPattern = {
    println ("toCellPattern: stared... ")
    val patternOptions = new ArrayBuffer[Array[PatternElem]]   // (Outer array: OR options (in most cases, there will be only one option).  Inner array: Individual elements of that pattern (e.g. *POS:JJ" + "cat")
    for (i <- 0 until cellPattern.length) {
      val elements = cellPattern(i).toPatternElems(tablestore)
      patternOptions.append(elements)
    }

    // Return
    inferenceengine.struct.CellPattern.mkCellPattern(columnName = colName, patternOptions = patternOptions.toArray)
  }


  override def toString():String = {
    val os = new StringBuilder

    os.append("(COLNAME: " + colName + ", ")
    os.append("PATTERN: " + cellPattern )
    os.append(")")

    // Return
    os.toString()
  }
}


case class CellRefExpr(colName:String, expr:Expr) extends Positional {

  override def toString():String = {
    val os = new StringBuilder

    os.append("CellRefExpr(colName: " + colName + ", ")
    os.append("Expr: " + expr)
    os.append(")")

    // Return
    os.toString()
  }
}

/*
class Expr extends Positional {

}

case class Number(value:Int) extends Expr

case class Operator(op:String, var left:Expr, var right:Expr) extends Expr

case class Identifier(name:String) extends Expr



*/

