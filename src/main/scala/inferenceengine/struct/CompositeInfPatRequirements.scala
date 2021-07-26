package inferenceengine.struct

import inferenceengine.iml.model.InfConstraint

/**
  * Created by user on 8/3/18.
  */

// Depricated -- use interpreter model classes
/*
class CompositeInfPatRequirements(
  // A list of references to other inference patterns that this composite pattern uses
  val infPatRefs:Array[InfPatRef],

  // A list of rows that should have the same UUID (i.e. be the same row) across the inference patterns
  val rowEquivs:Array[RowEquivalency]

                                 ) {

}


// Storage class for a reference to an inference pattern in a composite inference pattern.
// e.g. infpat x ChangeOfState
// for this, 'x' is the 'referenceName' (a variable name for this instance of the pattern), and 'ChangeOfState' is the 'patternName'
class InfPatRef(referenceName:String, patternName:String) {

    override def toString():String = {
      val os = new StringBuilder

      os.append("InfPatRef(referenceName: " + referenceName + " patternName: " + patternName + ")")

      // Return
      os.toString()
    }

}


// Storage class for an equivalence between rows in different inference pattern references
// e.g. rowequiv x.rowname1 = y.rowname2
// This means x.rowname must have the same UUID as y.rowname
// 'patternName1:x', 'rowName1:rowname1', 'patternName2:y', 'rowName2:rowname2'
class RowEquivalency(patternName1:String, rowName1:String, patternName2:String, rowName2:String) {

  override def toString():String = {
    val os = new StringBuilder

    os.append("RowEquivalency(patternName1: " + patternName1 + " rowName1: " + rowName1 + " <--> patternName2: " + patternName2 + " rowName2: " + rowName2 + ")")

    // Return
    os.toString()
  }

}
*/