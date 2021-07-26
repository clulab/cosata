package inferenceengine.struct

import scala.collection.mutable

/**
  * Class for storing remappings of variables (e.g. between names in an InferencePattern, and names in a State)
  * Created by user on 8/2/18.
  */

class Remap(val instances:RemapUni = new RemapUni) {

  def mergeHierarchy(nextLevel:Remap):Remap = {
    val out = new Remap( this.instances.mergeHierarchy( nextLevel.instances ))
    // Return
    out
  }

  /*
   * Equality/Subsets
   */
  // Unverified
  def hasSubset(in:Remap):Boolean = {
    if (this.instances.hasSubset(in.instances)) return true
    // Default return
    false
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append( instances.toString() )

    // Return
    os.toString()
  }
}

// Unidirectional
class RemapUni {
  val remap = mutable.Map[String, String]()

  def addRemap(find:String, replace:String) {
    remap(find) = replace
  }

  def getRemap(find:String):Option[String] = {
    if (remap.contains(find)) {
      Some( remap(find) )
    } else {
      None
    }
  }

  def size:Int = {
    remap.size
  }

  // Merge with a higher level map, so that (e.g.) if this level says
  //  a -> b
  // and the next level says
  //  b -> x
  // Then the merged will map
  //  a -> x
  def mergeHierarchy(nextLevel:RemapUni):RemapUni = {
    val out = new RemapUni

    // Step 1: Add in nextLevel remaps, checking for another level of indirection
    for (key <- nextLevel.remap.keySet) {
      val redirect = getRemap( nextLevel.getRemap(key).get )
      if (redirect.isDefined) {
        out.addRemap(key, redirect.get)
      } else {
        out.addRemap(key, nextLevel.getRemap(key).get)
      }
    }

    /*
    // Not required
    // Step 2: Add in current level remaps, ensuring that a higher level remap doesn't now exist
    for (key <- remap.keySet) {
      if (out.getRemap(key).isEmpty) {
        // This key doesn't exist in the output -- add it
        out.addRemap(key, remap(key))
      }
    }
    */

    // Return
    out
  }

  /*
   * Equality/Subsets
   */
  // Check to see whether 'in' is a subset of the current object
  // Unverified
  def hasSubset(in:RemapUni):Boolean = {
    val keysThis = this.remap.keySet
    val keysIn = in.remap.keySet

    val intersection = keysIn.intersect(keysThis)
    if (intersection.size != keysIn.size) return false

    for (key <- intersection) {
      if (in.getRemap(key).get != this.getRemap(key).get) return false
    }

    // If we reach here, the remaps are the same
    true
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("RemapUni[")
    val sorted = remap.keySet.toArray.sorted
    for (i <- 0 until sorted.length) {
      val key = sorted(i)
      os.append(key + " -> " + remap(key) )
      if (i < sorted.length-1) os.append(", ")
    }
    os.append("]")

    // Return
    os.toString()
  }

}

// Bidirectional
class RemapBi {
  val remap = mutable.Map[String, String]()

  def addRemap(name1:String, name2:String) {
    remap(name1) = name2
    remap(name2) = name1
  }

  def getRemap(name:String):Option[String] = {
    if (remap.contains(name)) {
      Some( remap(name) )
    } else {
      None
    }
  }

  def size:Int = {
    remap.size
  }

  /*
   * String
   */
  override def toString():String = {
    val os = new StringBuilder

    os.append("RemapBi[")
    val sorted = remap.keySet.toArray.sorted
    for (i <- 0 until sorted.length) {
      val key = sorted(i)
      os.append(key + " <-> " + remap(key) )
      if (i < sorted.length-1) os.append(", ")
    }
    os.append("]")

    // Return
    os.toString()
  }

}