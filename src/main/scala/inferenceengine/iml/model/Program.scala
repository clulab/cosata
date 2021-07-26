package inferenceengine.iml.model

/**
  * Storage class for an IML program
  * Created by peter on 7/9/18.
  */
class Program(val imports:List[ImportFileOrFolder], val functions:List[Function], val infpatterns:List[InfPattern], val statements:List[Statement]) {

  // Alternate constructor for blank program
  def this() = this(List.empty[ImportFileOrFolder], List.empty[Function], List.empty[InfPattern], List.empty[Statement])



  /*
   * Operators
   */
  def +(that:Program):Program = {
    new Program(this.imports ++ that.imports, this.functions ++ that.functions, this.infpatterns ++ that.infpatterns, this.statements ++ that.statements)
  }

  /*
   * String methods
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("Imports: \n")
    for (i <- 0 until imports.length) {
      println (i + ": \t" + imports(i))
    }
    os.append("\n")

    os.append("Functions: " + "\n")
    os.append("TODO \n")
    os.append("\n")

    os.append("Inference Patterns (size = " + infpatterns.length + "): " + "\n")
    for (i <- 0 until infpatterns.length) {
      os.append("Inference Pattern " + i + ": ")
      //infpatterns(i).toString()
      os.append( infpatterns(i).toString() + "\n")
      //os.append( infpatterns(i).prettyPrint( infpatterns(i) ) )
    }
    os.append("\n")

    os.append("Statements: " + "\n")
    for (i <- 0 until statements.length) {
      os.append( statements(i).toString() + "\n")
    }
    os.append("\n")

    // Return
    os.toString()
  }
}
