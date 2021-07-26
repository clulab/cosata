package inferenceengine.iml.states

import java.io.PrintWriter

import explanationgraph.{TableRow, TableStore}
import util.LexiconUtil

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * A storage class for a sequence of InferenceStates
  * Created by user on 11/25/18.
  */
class StateSequence {
  private val states = new ArrayBuffer[InferenceState]

  /*
   * Constructor
   */

  // Add a single blank InferenceState on construction
  addState()


  /*
   * Setters
   */

  // Add a blank InferenceState to the state sequence
  def addState() {
    // Step 1: Add new blank InferenceState
    states.append( new InferenceState )

    // Step 2: Clone instances from previous state to new state
    if (states.length > 1) {
      cloneInstances(states(states.length-2), states(states.length-1))
    }
  }

  // Add an externally generated state
  def addManualState(in:InferenceState) {
    states.append(in)
  }


  /*
   * Accessors
   */

  // Get the current (i.e. most recent) InferenceState
  def currentState:InferenceState = {
    return states.last
  }

  def numStates:Int = {
    return states.length
  }

  // Return a sorted list of all instance names used across all states
  def getAllInstanceNames():Array[String] = {
    // Step 1: Create a set of all instance names found across all states
    val names = mutable.Set[String]()
    for (state <- states) {
      for (instance <- state.instances) {
        names.add( instance.name )
      }
    }

    // Step 2: Return a sorted array of instance names
    names.toArray.sorted
  }

  // Get all the names of the states
  def getAllStateNames():Set[String] = {
    val out = mutable.Set[String]()
    for (state <- states) {
      out.add(state.name)
    }
    out.toSet
  }

  // Get all states with a given name
  def getStatesWithName(in:String):Array[InferenceState] = {
    val out = new ArrayBuffer[InferenceState]()
    for (i <- 0 until states.length) {
      if (states(i).name == in) {
        out.append(states(i))
      }
    }
    // Return
    out.toArray
  }

  def getStateIndicesWithName(in:String):Array[Int] = {
    val out = new ArrayBuffer[Int]()
    for (i <- 0 until states.length) {
      if (states(i).name == in) {
        out.append(i)
      }
    }
    // Return
    out.toArray
  }

  // Get state by array index
  def getStateByIndex(idx:Int):InferenceState = {
    states(idx)
  }

  /*
   * Misc. Control
   */

  // Clear the state history, and add a single blank state
  def resetStates() {
    // Clear all state history
    states.clear()

    // Add a single blank InferenceState
    addState()
  }


  /*
   * Supporting functions
   */

  // Clone all instances from one inference state to another inference state
  def cloneInstances(from:InferenceState, to:InferenceState) {
    for (instance <- from.instances) {
      //to.addInstance( instance.cloneDeep(instance.name) )
      to.addInstance( instance.cloneShallow(instance.name) )
    }
  }


  /*
   * Export
   */
  def exportHTML(filename:String, tablestore:TableStore):String = {
    val os = new StringBuffer()
    println ("* StateSequence: export started... (filename = " + filename + ")")

    // Header
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


    // Content
    for (stateNum <- 0 until states.length) {
      val state = states(stateNum)
      val stateName = state.name
      var stateColor = "black"
      if (stateName == InferenceState.STATE_UNNAMED) stateColor = "lightgrey"

      if (stateNum == 0) {
        // Case: First state (with no prior history to diff)
        os.append(exportOneState(state, None, name = "State " + stateNum + " <font color=" + stateColor + ">(" + stateName + ")</font>", tablestore))
      } else {
        // Case: Second state onward (with prior history to diff)
        os.append(exportOneState(state, Some(states(stateNum-1)), name = "State " + stateNum + " <font color=" + stateColor + ">(" + stateName + ")</font>", tablestore))
      }

      // Horizontal rule between states
      if (stateNum < states.length-1) {
        os.append("<br><hr><br>\n")
      }
    }


    os.append(" </div> \n")


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

    // Footer
    os.append("</body> \n")
    os.append("</html> \n")


    val pw = new PrintWriter(filename)
    pw.println(os.toString())
    pw.flush()
    pw.close()

    // Return
    os.toString()
  }


  def exportOneState(state:InferenceState, diffWithState:Option[InferenceState], name:String, tablestore:TableStore):String = {
    val os = new StringBuffer()

    val allInstanceNames = getAllInstanceNames()

    os.append("<div id=\"statename" + name + "\" style=\"overflow:hidden;\">\n")

    os.append("<div id=\"statename" + name + "instances\" style=\"overflow:hidden;\">\n")
    os.append("<h2>" + name + "</h2><br>\n")
    for (i <- 0 until allInstanceNames.length) {
      val name = allInstanceNames(i)
      val instance_ = state.getInstanceSafe(name)           // It's possible for some instances to not be defined at a given state (i.e. they were defined later) -- so we have to check to be safe
      if (instance_.isDefined) {
        val instance = instance_.get
        val propertyValues = instance.propertyValues

        // Start instance box
        os.append("<div id=\"" + i + "\" style=\"width:250px; float:left; margin:10px; padding:10px; background-color:#F5F5F5; font-size: 14px; border-radius: 4px; box-shadow: 0 3px 6px rgba(0,0,0,0.16), 0 3px 6px rgba(0,0,0,0.23);\">\n")
        os.append("<style> ul { padding-left: 20px; } </style>")
        // Get (human-readable) name from properties
        val valuePropName = propertyValues.get("name")
        if (valuePropName.isDefined) {
          os.append("<b>" + valuePropName.get.getValue() + "</b><br>\n")
        } else {
          os.append("<i>[No name defined]</i><br>\n")
        }

        // Also report object instance name and kindof
        val kindofStr = LexiconUtil.lexiconIdxsToStr(instance.kindof, tablestore.lexicon)
        os.append("<b>" + name + "</b> [kindof: " + kindofStr + "]<br>\n")

        // List all property/value pairs
        for (key <- propertyValues.keySet) {
          // Get current property value
          val value = propertyValues.get(key).get.getValue
          val valueFormatted = propertyValues.get(key).get.getValueFormatted()

          // Allow colour-coded diffs between the current property value and any property valu found in diffWithState
          var diffChanged: Boolean = false
          if (diffWithState.isDefined) {
            var diffValue: Option[String] = None
            val diffInstance = diffWithState.get.getInstanceSafe(name)
            if (diffInstance.isDefined) {
              val diffProperty = diffInstance.get.getProperty(key)
              if (diffProperty.isDefined) {
                diffValue = Some(diffProperty.get.getValue())
              }
              if (value != diffValue.getOrElse("no_value_assigned")) {
                diffChanged = true
              }
            } else {
              // If an instance is newly defined in this state, then perform the highlighting for all the property/value pairs.
              diffChanged = true
            }
          }

          // Write HTML
          if (diffChanged) {
            // Changed value -- highlight
            val highlightColor = "red"
            //val highlightColor = "MediumSlateBlue"
            //val highlightColor = "DarkOrange"
            os.append("<font color=\"" + highlightColor + "\">" + key + " : " + valueFormatted + "</font><br>")
          } else {
            // Unchanged value -- no highlight
            os.append(key + " : " + valueFormatted + "<br>")
          }

        }

        os.append("<hr>\n")
        os.append("<ul>\n")
        // List all attached table rows
        val attachedRows = instance.getAttachedTableRows()
        for (uuid <- attachedRows) {
          val row = tablestore.getRowByUID(uuid)
          if (row.uid != TableRow.DEFAULT_ROW_UUID) {
            // Row found
            os.append("<li>" + row.toStringSentWithUID(delim = "<i>") + "</i>\n")
          } else {
            // Row not found
            os.append("<li>  * Row with UUID " + uuid + " not found! * \n")
          }
        }
        os.append("</ul>\n")

        // End instance box
        os.append("</div>\n")
      }

    }
    os.append("</div>\n<br>\n")

    os.append("<div id=\"statename" + name + "text\" style=\"overflow:hidden;\">\n")
    os.append("<br>\n")

    // Inference patterns executed
    os.append("Inference Patterns Executed (Short Description):<br> \n")
    os.append("<table class=\"log\">\n")

    // Header
    os.append("<tr class=\"log\" style=\"background-color: #cce4ff\">")
    os.append("<th class=\"log\">#</th>")
    os.append("<th class=\"log\">Name</th>")
    os.append("<th class=\"log\">Short Description</th>")
    os.append("<th class=\"log\">Flags</th>")
    os.append("</tr>\n")


    for (i <- 0 until state.inferencePatternsExecuted.size) {
      /*
      // Alternating colours
      if (i % 2 == 1) {
        os.append("<tr style=\"background-color: #eeeeee\">")
      } else {
        os.append("<tr style=\"background-color: #ffffff\">")
      }
      */
      //## Do not include autocalls (generates very long logfiles)
      if (!state.inferencePatternsExecuted(i).wasAutoCall) {

        // Flag-related colours
        if (state.inferencePatternsExecuted(i).wasNestedCall) {
          os.append("<tr class=\"log\" style=\"background-color: #eeeeee\">")
        } else if (state.inferencePatternsExecuted(i).wasAutoCall) {
          os.append("<tr class=\"log\" style=\"background-color: #dddddd\">")
        } else {
          os.append("<tr class=\"log\" style=\"background-color: #ffffff\">")
        }


        if (!state.inferencePatternsExecuted(i).wasAutoCall) {
          os.append("<td class=\"log\">" + i + "</td>\n")

          var indent: String = ""
          if (state.inferencePatternsExecuted(i).wasNestedCall || state.inferencePatternsExecuted(i).wasAutoCall) indent = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
          os.append("<td class=\"log\">" + indent + state.inferencePatternsExecuted(i).name + "</td>\n");
          os.append("<td class=\"log\">" + state.inferencePatternsExecuted(i).shortDescription + "</td>")

          val flags = new ArrayBuffer[String]
          if (state.inferencePatternsExecuted(i).wasNestedCall) flags.append("nested call")
          if (state.inferencePatternsExecuted(i).wasAutoCall) flags.append("auto call")
          if (state.inferencePatternsExecuted(i).notes.length > 0) flags.append(state.inferencePatternsExecuted(i).notes)
          os.append("<td class=\"log\">" + flags.mkString(", ") + "</td>")

          os.append("</tr>\n")
        }
      }
    }

    os.append("</table>\n")

    os.append("<br>\n")

    os.append("<button onclick=\"accordion('" + name + "state')\" class=\"w3-button w3-block w3-blue w3-left-align\"> ")
    os.append( "Inference Patterns Executed (detailed)")
    os.append(" </button> \n")

    os.append("<div id=\"" + name + "state\" class=\"w3-container w3-hide\"> \n")
    os.append("<blockquote> \n")
    for (i <- 0 until state.inferencePatternsExecuted.length) {
      if (!state.inferencePatternsExecuted(i).wasAutoCall) {
        if (i > 0) os.append("<hr><br><br>")
        // Name
        os.append( "<b><h2>Inference Pattern: " + state.inferencePatternsExecuted(i).name )
        if (state.inferencePatternsExecuted(i).wasNestedCall) os.append(" (nested call)")
        if (state.inferencePatternsExecuted(i).wasAutoCall) os.append(" (auto call)")
        os.append("</h2></b><br>")

        // Remapping:
        val instRemap = state.inferencePatternsExecuted(i).instRemap
        os.append("instRemap: " + state.inferencePatternsExecuted(i).instRemap.toString() + "<br>")
        val (valid, comboIdx) = state.inferencePatternsExecuted(i).patternMatch.checkIfValidRemap( instRemap.instances )
        os.append("valid: " + valid + "<br>")
        os.append("comboIdx: " + comboIdx + "<br>")


        // Detailed display:
        os.append( state.inferencePatternsExecuted(i).patternMatch.toHTMLString(lexicon = tablestore.lexicon, constraintHighlight = comboIdx) )


      }
    }
    os.append("</blockquote> \n")
    os.append("</div>\n")
    os.append("<br>\n")



    // Extra explanatory text
    os.append("Extra Explanatory Text:<br>\n")
    os.append("<table class=\"log\">\n")

    // Header
    os.append("<tr class=\"log\" style=\"background-color: #cce4ff\">")
    os.append("<th class=\"log\">#</th>")
    os.append("<th class=\"log\">FromPattern</th>")
    os.append("<th class=\"log\">Text</th>")
    os.append("</tr>\n")


    for (i <- 0 until state.extraExplanatoryText.length) {
      // Alternating colours
      if (i % 2 == 1) {
        os.append("<tr class=\"log\" style=\"background-color: #eeeeee\">")
      } else {
        os.append("<tr class=\"log\" style=\"background-color: #ffffff\">")
      }

      os.append("<td class=\"log\">" + i + "</td>\n")
      os.append("<td class=\"log\">" + state.extraExplanatoryText(i).source + "</td>\n")
      os.append("<td class=\"log\">" + state.extraExplanatoryText(i).text + "</td>\n")

      os.append("</tr>\n")
    }
    os.append("</table>\n")


    os.append("</div>\n<br>\n")
    // Return
    os.toString()
  }


  /*
   * String methods
   */

  override def toString():String = {
    val os = new StringBuilder

    os.append(" ---------------------------------------------------------------------------- \n")
    os.append(" ---------------------------------------------------------------------------- \n")
    os.append("Number of states: " + numStates + "\n")

    for (i <- 0 until states.length) {
      os.append("State " + i + ":\n")
      os.append(states(i).toString(includeAutoPatterns = false))
      os.append("\n\n")
      os.append(" ---------------------------------------------------------------------------- \n")
      os.append("\n\n")
    }

    os.append(" ---------------------------------------------------------------------------- \n")
    os.append(" ---------------------------------------------------------------------------- \n")


    // Return
    os.toString()
  }

}
