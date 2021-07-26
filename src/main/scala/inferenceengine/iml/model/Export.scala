package inferenceengine.iml.model

/**
  * Created by user on 8/2/18.
  */
class Export {

}

case class ExportInferencePatternsHTML(filename:String) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder
    os.append("ExportInferencePatternsHTML(filename: " + filename + ")")
    // Return
    os.toString()
  }

}

case class ExportInferencePatternsJSON(filename:String) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder
    os.append("ExportInferencePatternsJSON(filename: " + filename + ")")
    // Return
    os.toString()
  }

}

case class ExportTableStoreHTML(filename:String) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder
    os.append("ExportTableStoreHTML(filename: " + filename + ")")
    // Return
    os.toString()
  }

}

case class ExportStateSpaceHTML(filename:String) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder
    os.append("ExportStateSpaceHTML(filename: " + filename + ")")
    // Return
    os.toString()
  }

}

case class ExportInferencePatternDebugHTML(path:String) extends Statement {

  override def toString(): String = {
    val os = new StringBuilder
    os.append("ExportInferencePatternDebugHTML(path: " + path + ")")
    // Return
    os.toString()
  }

}