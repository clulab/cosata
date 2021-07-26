package inferenceengine.struct

/**
  * A sketch of a storage class for the value of a property.  The value can be either numeric, discrete (string), or ranges, with modifiers.
  * Created by user on 7/24/18.
  */
class PropertyValue(var value:String = "", var unit:String = "") {

  private var _isNumeric:Boolean = false
  private var _isDiscrete:Boolean = false
  private var _hasBeenSet:Boolean = false

  /*
   * Accessors
   */
  def isNumeric():Boolean = _isNumeric

  def isDiscrete():Boolean = _isDiscrete

  def getValue():String = {
    value
  }

  def getValueDouble():Option[Double] = {
    try {
      Some( value.toDouble )
    } catch {
      case e:NumberFormatException => None
    }
  }

  def getUnit():String = unit

  def hasUnit():Boolean = {
    if (unit.length > 0) return true
    // Default return
    false
  }

  // Return true if this property value has been set at least once, false if it is uninitialized
  def hasBeenSet():Boolean = _hasBeenSet

  /*
   * Setters
   */
  def setDiscrete(v:String) = {
    value = v
    _isDiscrete = true
    _isNumeric = false
    _hasBeenSet = true
  }

  def setNumeric(v:String) = {
    value = v
    _isDiscrete = false
    _isNumeric = true
    _hasBeenSet = true
  }

  def setUnit(u:String) = {
    unit = u
  }

  /*
   * String methods
   */
  // Retuns a slightly formatted/rounded value, useful for human readability, but should not be used for comparisons.
  def getValueFormatted():String = {
    // If a numeric value, then truncate that numeric value for readability
    if (this.getValueDouble().isDefined) {
      return this.getValueDouble().get.formatted("%10.4f")
    }
    // Otherwise, a string value
    return this.getValue()
  }


  /*
   * Cloning
   */

  /*
  // not currently working
  override def clone():PropertyValue = {
    new PropertyValue(value = new String(this.value), unit = new String(this.unit))
  }
  */
}
