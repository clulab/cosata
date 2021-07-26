package inferenceengine.iml.model

/**
  * Created by peter on 7/10/18.
  */
case class GenerateRow(rowrefExpr:RowRefExpr, variableNameForUUID:Option[String]) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("generateRow([")
    os.append( rowrefExpr.toString )
    os.append("], variableNameForUUID: " + variableNameForUUID + ")")

    // Return
    os.toString()
  }

}


case class RemoveRowUUID(uuidExpr:Expr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("removeRowUUID(uuidExpr: " + uuidExpr + ")")

    // Return
    os.toString()
  }

}

case class RemoveRowsRowRef(rowref:RowRefExpr) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("removeRowsRowRef(rowref: " + rowref + ")")

    // Return
    os.toString()
  }

}