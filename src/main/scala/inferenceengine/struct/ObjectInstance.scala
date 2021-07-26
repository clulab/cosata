package inferenceengine.struct

import explanationgraph.{TableRow, TableStore}

import scala.collection.mutable
import ObjectInstance._

import scala.collection.mutable.ArrayBuffer

/**
  * A storage class for an instance of an object in the simulation
  * Created by user on 7/24/18.
  */
class ObjectInstance(
  // A name for this instance
  val name:String,

  // The taxonomic type of this instance (e.g. 'animal'). Stored as lexicon lemma references.
  val kindof:Array[Int],

  // A reference to the tablestore, for taxonomic and property information
  val tablestore:TableStore) {


  // Property-value pairs
  val propertyValues = mutable.Map[String, PropertyValue]()

  // List of table rows (acting as relational information) attached to this instance
  val tableRowsAttached = new ArrayBuffer[String]

  // Lexicon
  val lexicon = tablestore.lexicon

  /*
   * Cloning
   */
  // Mostly shallow clone except for name/kindof -- property references are still references to original,
  // so modifying properties will affect both original and clone.
  def cloneShallow(newName:String):ObjectInstance = {
    val out = new ObjectInstance(newName, kindof, tablestore)
    for (key <- propertyValues.keySet) {
      out.setProperty(key, this.getProperty(key).get)
    }
    for (elem <- tableRowsAttached) {
      //out.att
    }

    // Return
    out
  }

/*
  def cloneDeep(newName:String):ObjectInstance = {
    val out = new ObjectInstance(newName, kindof, tablestore)
    for (key <- propertyValues.keySet) {
      out.setProperty(key, this.getProperty(key).get.clone())
    }
    // Return
    out
  }
*/



  /*
   * Properties
   */
  def getProperty(property:String):Option[PropertyValue] = {
    if (propertyExists(property)) {
      return Some( propertyValues(property) )
    }

    // Default return
    None
  }

  def setProperty(property:String, value:PropertyValue) {
    propertyValues(property) = value
  }

  // Check if a property exists
  def propertyExists(property:String):Boolean = {
    if (propertyValues.contains(property)) return true

    // Default return
    false
  }


  /*
   * Automatically infer base properties
   */

  // TODO: perform taxonomic traversal, so that if A is a kind of X, and X is also a kind of Y, then A will get all properties from X and Y (etc).

  // Insert the (unset) properties for something that is a kindOf something (X).
  def insertProperties(kindOfX:Array[Int]) {
    val propTable = tablestore.getTableByName(TABLENAME_PROPERTIES)

    if (propTable.isEmpty) {
      throw new RuntimeException("ERROR: PropTable '" + TABLENAME_PROPERTIES + "' could not be found. ")
    }

    for (rowIdx <- 0 until propTable.get.numRows()) {
      val row = propTable.get.getRowByIdx(rowIdx)

      val numAlts = row.getCellNumAlternatives(PROPERTIES_OBJECTFIELD)
      for (altIdx <- 0 until numAlts) {
        val objField = row.getCellLemmasAlt(PROPERTIES_OBJECTFIELD, altIdx, onlyContentTags = true)
        //println ("Content of object field: " + objField.mkString(",") + " (" + convertLemmaArrayToString(objField) + ")")

        if (objField.deep == kindOfX.deep) {
          val numAlts1 = row.getCellNumAlternatives(PROPERTIES_PROPFIELD)
          for (altIdx1 <- 0 until numAlts1) {
            val propertyField = row.getCellLemmasAlt(PROPERTIES_PROPFIELD, altIdx1, onlyContentTags = true)
            //println ("Content of property field: " + propertyField.mkString(",") + " (" + convertLemmaArrayToString(propertyField) + ")")
            val propertyNameStr = convertLemmaArrayToString(propertyField)

            //println ("Found property '" + propertyNameStr + "' of object '" + convertLemmaArrayToString(objField) + "'" )
            // Generate a blank, unset property value
            val value = new PropertyValue

            if (!propertyExists(propertyNameStr)) {
              // If this is a new property, instantiate the property.  If not a new property, do nothing -- the property may already be set.
              setProperty( propertyNameStr, value )
            }
          }
        }
      }
    }

  }


  /*
   * Attaching table rows
   */
  def attachTableRow(uuid:String) {
    if (!tableRowsAttached.contains(uuid)) {
      tableRowsAttached.append(uuid)
    }
  }

  def detachTableRow(uuid:String) {
    for (i <- 0 until tableRowsAttached.length) {
      if (tableRowsAttached(i) == uuid) {
        tableRowsAttached.remove(i)
        return
      }
    }
  }

  def getAttachedTableRows():Array[String] = {
    tableRowsAttached.toArray
  }


  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder()

    os.append("Instance Name: " + name + " (KINDOF: " + getKindOfStr() + ")\n")
    os.append("Properties:\n")

    val propertyNames = propertyValues.keySet.toArray.sorted
    for (i <- 0 until propertyNames.length) {
      val propertyName = propertyNames(i)
      os.append("\t\t" + i + "\t" + propertyName.formatted("%20s") + "\t")
      val value = getProperty(propertyName)
      if (value.isDefined) {
        if (value.get.hasBeenSet()) {
          os.append(value.get.getValue())
          if (value.get.hasUnit()) {
            os.append(" (Unit = " + value.get.getUnit() + ")")
          }
        } else {
          os.append("*Unset*")
        }
      } else {
        os.append("*Uninstantiated*")
      }
      os.append("\n")
    }


    os.append("Attached Table Rows:\n")
    //## os.append( "(" + tableRowsAttached.mkString(", ") + ")")    // debug
    val attachedRows = getAttachedTableRows()
    if (attachedRows.length == 0) {
      os.append("*No attached table rows*\n")
    } else {
      for (uuid <- attachedRows) {
        val row = tablestore.getRowByUID(uuid)
        if (row.uid != TableRow.DEFAULT_ROW_UUID) {
          // Row found
          os.append("  " + row.toStringSentWithUID() + "\n")
        } else {
          // Row not found
          os.append("  * Row with UUID " + uuid + " not found! * \n")
        }
      }
    }
    os.append("\n")


    // Return
    os.toString()
  }



  def getKindOfStr():String = {
    convertLemmaArrayToString( kindof )
  }


  // Convert an array of lemma lexicon indices into a plain text string
  def convertLemmaArrayToString(in:Array[Int]):String = {
    val os = new StringBuilder()

    for (lemmaIdx <- in) {
      os.append( lexicon.get(lemmaIdx) + " ")
    }

    // Return
    os.toString().trim()
  }

}



object ObjectInstance {
  val TABLENAME_TAXONOMIC     =   "KINDOF"

  val TABLENAME_PROPERTIES    =   "PROP-GENERIC"
  val PROPERTIES_PROPFIELD    =   "PROPERTY"
  val PROPERTIES_OBJECTFIELD  =   "OBJECT"

}