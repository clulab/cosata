package inferenceengine

import inferenceengine.struct._
import explanationgraph.{TableRow, TableStore}
import TableRow.{MODE_ANY, MODE_LEMMA, MODE_TLEMMA, MODE_TWORD, MODE_WORD}
import inferenceengine.iml.model.{InfConstraint, InfInstanceReq, Statement, Str}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

/**
  * Created by peter on 6/19/18.
  */
class InferenceEngineEntryPoint {

}


object InferenceEngineEntryPoint {
  val logger = LoggerFactory.getLogger(classOf[InferenceEngineEntryPoint])


  def main(args:Array[String]) {

    logger.info(" * InferenceEngineEntryPoint: Started...")
    /*
    // Step 1: Check that arguments and a properties file were specified
    // e.g. " -props myprops.properties"
    if ((args.length == 0)) {
      //printUsage()
      println ("No properties file specified.")
      System.exit(1)
    }
    val props = StringUtils.argsToProperties(args)
    */


    // Step 2: Load tablestore
    //var pathAnnotation = "annotation/expl-minitablestore-export-2018-06-29-164137/"
    //var pathAnnotation = "annotation/expl-minitablestore-export-2018-06-22-160637/"
    var pathAnnotation = "annotation/expl-minitablestore-current/"
    //var pathAnnotation = "annotation/expl-minitablestore-test/"
    val tablestore = new TableStore(pathAnnotation + "tableindex-mini.txt")
    println ( tablestore.tables( tablestore.UIDtoTableLUT("a0b2-a45f-01e1-4bf4") ).name )
    println ( tablestore.getRowByUID("a0b2-a45f-01e1-4bf4") )


  }


}


