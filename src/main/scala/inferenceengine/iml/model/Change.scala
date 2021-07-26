package inferenceengine.iml.model

/**
  * Created by peter on 7/27/18.
  */
case class Change(elems:List[ChangeElem]) {

  override def toString(): String = {
    val os = new StringBuilder

    os.append("Change(elems: " + elems.toString() + ")")

    // Return
    os.toString()

  }

}
