package inferenceengine.iml.model

import edu.arizona.sista.struct.Lexicon
import util.LexiconUtil

/**
  * Created by user on 7/25/18.
  */
case class InstanceDefinition(name:String, kindof:Expr) extends Statement {

  // Optionally, also store 'name' and 'kindof' as lexicon references, for speed.
  var nameLexicon:Array[Int] = Array.empty
  //var kindofLexicon:Array[Int] = Array.empty

  // Populate lexicon references.
  def populateLexiconRefs(lexicon:Lexicon[String]) {
    nameLexicon = LexiconUtil.strToLexiconIdxs(name, lexicon)
    //kindofLexicon = LexiconUtil.strToLexiconIdxs(kindof, lexicon)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstanceDefinition(name: " + name + ", kindof:" + kindof + ")")

    // Return
    os.toString()
  }

}

case class InstanceRequirement(name:String, kindof:String) extends StatementInf {

  // Optionally, also store 'name' and 'kindof' as lexicon references, for speed.
  var nameLexicon:Array[Int] = Array.empty
  var kindofLexicon:Array[Int] = Array.empty

  // Populate lexicon references.
  def populateLexiconRefs(lexicon:Lexicon[String]) {
    nameLexicon = LexiconUtil.strToLexiconIdxs(name, lexicon)
    kindofLexicon = LexiconUtil.strToLexiconIdxs(kindof, lexicon)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstanceRequirement(name: " + name + ", kindof:" + kindof + ")")

    // Return
    os.toString()
  }

}


case class InstantiateCommonProperties(name:String, kindof:Expr) extends Statement {

  // Optionally, also store 'name' and 'kindof' as lexicon references, for speed.
  var nameLexicon:Array[Int] = Array.empty
  //var kindofLexicon:Array[Int] = Array.empty

  // Populate lexicon references.
  def populateLexiconRefs(lexicon:Lexicon[String]) {
    nameLexicon = LexiconUtil.strToLexiconIdxs(name, lexicon)
    //kindofLexicon = LexiconUtil.strToLexiconIdxs(kindof, lexicon)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstantiateCommonProperties(name: " + name + ", kindof:" + kindof + ")")

    // Return
    os.toString()
  }

}


case class InstantiateWithProperties(name:String, kindof:Expr, properties:List[String]) extends Statement {

  // Optionally, also store 'name' and 'kindof' as lexicon references, for speed.
  var nameLexicon:Array[Int] = Array.empty
  //var kindofLexicon:Array[Int] = Array.empty
  var propertiesLexicon:Array[Int] = Array.empty

  // Populate lexicon references.
  def populateLexiconRefs(lexicon:Lexicon[String]) {
    nameLexicon = LexiconUtil.strToLexiconIdxs(name, lexicon)
    //kindofLexicon = LexiconUtil.strToLexiconIdxs(kindof, lexicon)
    propertiesLexicon = LexiconUtil.strToLexiconIdxs(properties.mkString(" "), lexicon)
  }

  override def toString():String = {
    val os = new StringBuilder

    os.append("InstantiateWithProperties(name: " + name + ", kindof:" + kindof + ", properties:" + properties.mkString(",") + ")")

    // Return
    os.toString()
  }

}