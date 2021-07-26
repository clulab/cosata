package inferenceengine.iml.visualization

import java.io.PrintWriter

import explanationgraph.TableStore
import inferenceengine.iml.runtime.Interpreter
import inferenceengine.struct.InferencePattern

import scala.collection.mutable.ArrayBuffer

/**
  * Created by peter on 7/11/18.
  */
class IMLHtmlExport(tablestore:TableStore) {
  val inferencePatterns = new ArrayBuffer[InferencePattern]


  /*
   * Adding data
   */

  def addInferencePatterns(in:Array[InferencePattern]): Unit = {
    inferencePatterns.insertAll( inferencePatterns.length, in )
  }


  /*
   * Generation
   */

  def generateInferencePatternHTML(includeAutoPatterns:Boolean = false, highlightPatterns:Set[String] = Set[String](), maxPatterns:Int = 200):String = {
    val os = new StringBuilder

    for (i <- 0 until inferencePatterns.length) {
      val infPattern = inferencePatterns(i)
      if ((!Interpreter.isAutoPattern(infPattern)) || (includeAutoPatterns)) {

        val name = infPattern.name

        // Rows
        if (!infPattern.isPopulated) {
          os.append("<button onclick=\"accordion('" + name + "rows')\" class=\"w3-button w3-block w3-light-grey w3-left-align\"> ")
          os.append(name + " (unpopulated)")
          os.append(" </button> \n")

        } else {

          var buttonColor = "w3-light-grey"
          if (highlightPatterns.contains(infPattern.name)) {
            buttonColor = "w3-blue"
          }

          /*
          os.append("<button onclick=\"accordion('" + name + "rows')\" class=\"w3-button w3-block " + buttonColor + " w3-left-align\"> ")
          os.append(name + " rows (" + infPattern.rowPatternMatches.length + " matches)")
          os.append(" </button> \n")

          os.append("<div id=\"" + name + "rows\" class=\"w3-container w3-hide\"> \n")
          os.append("<blockquote> <pre> \n")
          os.append(infPattern.candidateRowsToString(tablestore.lexicon) + "\n")
          os.append("</pre> </blockquote> \n")
          os.append("</div>\n")
           */


          // Inference patterns

          println (" * Check number runnable")
          // Count number of runnable matches
          var numRunnable: Int = 0
          for (i <- 0 until infPattern.fullPatternMatches.length) {
            if (infPattern.fullPatternMatches(i).canContraintsBeMet()) {
              numRunnable += 1
            }
          }

          os.append("<button onclick=\"accordion('" + name + "infpatterns')\" class=\"w3-button w3-block " + buttonColor + " w3-left-align\"> ")
          os.append(name + " inference patterns (" + infPattern.fullPatternMatches.length + " matches / " + numRunnable + " meet(s) requirements). ")
          if (infPattern.description.length > 0) os.append(" (description = " + infPattern.description + ")")
          os.append(" </button> \n")

          os.append("<div id=\"" + name + "infpatterns\" class=\"w3-container w3-hide\"> \n")
          os.append("<blockquote> \n")

          println (" * Generate HTML ")
          os.append(infPattern.validInferencePatternsToHTMLString(tablestore.lexicon, maxPatterns) + "\n")
          println (" * Generate HTML completed ")

          os.append("</blockquote> \n")
          os.append("</div>\n")
        }

      }
    }


    // Add the accordion script
    os.append("<script>\n")
    os.append("function accordion(id) { \n")
    os.append("  var x = document.getElementById(id);\n")
    os.append("  if (x.className.indexOf(\"w3-show\") == -1) { \n")
    os.append("    x.className += \" w3-show\"; \n")
    os.append("  } else { \n")
    os.append("    x.className = x.className.replace(\" w3-show\", \"\"); \n")
    os.append("  }\n")
    os.append("} \n")
    os.append("</script>\n")


    // Return
    os.toString()
  }


  /*
   * Output
   */


  def generateHTML(filename:String, highlightPatterns:Set[String] = Set[String]()): Unit = {
    val os = new StringBuilder

    println (" * generateHTML: started... (filename = " + filename + ")")

    os.append("<html> \n")
    os.append("<head> \n")
    os.append("<title> inference pattern export </title> \n")
    os.append(" <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\"> \n")
    os.append("<style>")
    os.append("table.log, th.log, td.log {\n    border: 1px solid black;\n border-collapse: collapse;\n padding: 5px;\n}")
    os.append("table.log#t01 tr:nth-child(even) {\n    background-color: #eee;\n}\ntable.log#t01 tr:nth-child(odd) {\n    background-color: #fff;\n}\ntable.log#t01 th {\n   background-color: #cce4ff;\n}")
    os.append("</style>")
    os.append("</head> \n")
    os.append("<body> \n")
    os.append(" <div class=\"w3-container\"> \n")


    // Generate HTML
    os.append( generateInferencePatternHTML(includeAutoPatterns = false, highlightPatterns) )


    os.append(" </div> \n")
    os.append("</body> \n")
    os.append("</html> \n")

    // Export
    val pw = new PrintWriter(filename)
    pw.println( os.toString() )
    pw.close()

    println (" * generateHTML: export completed... ")

  }


}
