package inferenceengine.iml.model

import explanationgraph.TableStore
import inferenceengine.struct.PatternElem

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.Positional

/**
  * Created by peter on 7/9/18.
  */

class CellExprElem extends Positional {

  // TODO: placeholder
  def toPatternElem(tablestore:TableStore):PatternElem = {
    throw new RuntimeException("CellExprElem: toPatternElem() called: this should never happen. ")
    PatternElem.mkVariable("BLANK", isOptional = false, isRelaxable = false)
  }

}

// Plain text
case class CellText(text:String, isOptional:Boolean, isRelaxable:Boolean) extends CellExprElem {

  override def toPatternElem(tablestore:TableStore):PatternElem = {
    PatternElem.mkLexicalPattern(text, tablestore.lexicon, isOptional=isOptional, isRelaxable=isRelaxable)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("[TEXT:" + text + " isOptional:" + isOptional + " isRelaxable:" + isRelaxable + "]")

    // Return
    os.toString()
  }
}

// A cell content variable
case class CellVariable(name:String, isOptional:Boolean, isRelaxable:Boolean) extends CellExprElem {

  override def toPatternElem(tablestore:TableStore):PatternElem = {
    PatternElem.mkVariable(name, isOptional=isOptional, isRelaxable=isRelaxable)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("[VAR:" + name + " isOptional:" + isOptional + " isRelaxable: " + isRelaxable + "]")

    // Return
    os.toString()
  }

}


case class CellPattern(var elements:List[CellExprElem]) extends Positional {

  def toPatternElems(tablestore:TableStore):Array[PatternElem] = {
    val out = new ArrayBuffer[PatternElem]
    println ("toPatternElems: Started... ")

    println (elements.mkString(", "))

    // convert each element into a PatternElement
    for (elem <- elements) {
      println ("elem: " + elem.toString)
      out.append( elem.toPatternElem(tablestore) )
    }

    // Return
    out.toArray
  }

  override def toString():String = {
    val os = new StringBuilder

    for (i <- 0 until elements.length) {
      os.append(elements(i).toString)
      if (i < elements.length-1) os.append(" ")
    }

    // Return
    os.toString()
  }

}