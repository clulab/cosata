package inferenceengine.iml.model

/**
  * Created by user on 8/2/18.
  */
// Execute a specific inference pattern
// Note: Only one of patternMatchIdx, patternHashcode, OR patternConditions should be populated.
case class ExecuteInfPattern(patternName:String, patternMatchIdx:Option[Int], patternHashcode:Option[String], patternConditions:Option[List[ConditionExpr]], remappings:InstRemap, temporalBlocksEnabled:Option[ExecTemporalBlockList]) extends Statement {


  override def toString():String = {
    val os = new StringBuilder

    os.append("ExecuteInfPattern(patternName: " + patternName + " patternMatchIdx: " + patternMatchIdx + "patternHashcode: " + patternHashcode + "patternConditions: " + patternConditions.mkString(", ") + " remapping: " + remappings + " temporalBlocksEnabled: " + temporalBlocksEnabled.getOrElse("None") + ")")


    // Return
    os.toString()
  }

}

// Note, this should mirror the parameters in ExecuteInfPattern
case class MeetsRequirementsInfPattern(patternName:String, patternMatchIdx:Option[Int], patternHashcode:Option[String], patternConditions:Option[List[ConditionExpr]], remappings:InstRemap, temporalBlocksEnabled:Option[ExecTemporalBlockList]) extends Expr {

  override def toString():String = {
    val os = new StringBuilder

    os.append("MeetsRequirementsInfPattern(patternName: " + patternName + " patternMatchIdx: " + patternMatchIdx + "patternHashcode: " + patternHashcode + "patternConditions: " + patternConditions.mkString(", ") + " remapping: " + remappings + " temporalBlocksEnabled: " + temporalBlocksEnabled.getOrElse("None") + ")")


    // Return
    os.toString()
  }

}



// Execute all automaticly-triggering inference patterns
case class ExecuteAutoPatterns() extends Statement {

  override def toString():String = {
    val os = new StringBuilder

    os.append("ExecuteAutoPatterns()")

    // Return
    os.toString()
  }

}