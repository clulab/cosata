package inferenceengine.iml.model

/**
  * Created by user on 8/15/18.
  */
class ImportFileOrFolder() extends Statement {

}

case class ImportFile(filename:String) extends ImportFileOrFolder {

  override def toString():String = {
    val os = new StringBuilder

    os.append("ImportFile(filename: " + filename + ")")

    // Return
    os.toString()
  }

}

case class ImportFolder(path:String) extends ImportFileOrFolder {

  override def toString():String = {
    val os = new StringBuilder

    os.append("ImportFolder(path: " + path + ")")

    // Return
    os.toString()
  }

}
