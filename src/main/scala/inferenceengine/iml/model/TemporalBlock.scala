package inferenceengine.iml.model

/**
  * Created by user on 9/6/18.
  */
case class TemporalBlock(mode:String, statements:List[Statement]) extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("TemporalBlock(Mode: " + mode + "):\n")
    os.append("\tStatements:\n")
    for (statement <- statements) {
      os.append("\t\t" + statement + "\n")
    }

    // Return
    os.toString()

  }


}

