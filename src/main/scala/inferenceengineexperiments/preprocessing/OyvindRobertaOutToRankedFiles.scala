package inferenceengineexperiments.preprocessing

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.util.parsing.json._

// JSON parsing bits from https://stackoverflow.com/questions/4170949/how-to-parse-json-in-scala-using-standard-scala-classes

class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

object M extends CC[Map[String, Any]]
object L extends CC[List[Double]]
object S extends CC[String]
object D extends CC[Double]
object B extends CC[Boolean]


class OyvindRobertaOutLine(val qid:String, val rowUUID:String, val rowText:String, val label:Double, val labelPred:Double, val labelLogits:Array[Double], val labelProbs:Array[Double]) {

  def getProbCorrect():Double = {
    labelProbs(1)
  }

  def toTSVLine(delim:String = "\t"):String = {
    val os = new StringBuilder

    os.append(rowUUID + delim)
    os.append(rowText + delim)
    os.append(getProbCorrect() + delim)
    os.append(labelLogits(1) + delim)
    os.append(label + delim)

    // Return
    os.toString()
  }

}


object OyvindRobertaOutToRankedFiles {
  var count:Int = 0

  def loadLines(filename:String, outPath:String, fileIdx:Int = -1): Unit = {
    println (" * Loading " + filename)

    var hasMultiple:Boolean = false
    var lineCount:Int = 0

    var curQID:String = ""
    val out = new ArrayBuffer[OyvindRobertaOutLine]

    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {

      //val parsed = JSON.parseFull(line).getOrElse(0).asInstanceOf[Map[String,String]]
      //println (parsed.toString())
      val idRaw = line.substring(0, line.lastIndexOf("__"))
      val extraUnderscoreIdx = idRaw.indexOf("__")
      val idDashCount = idRaw.count(_ == '-')

      /*
      if (idRaw.contains("MCAS_2006_9_9")) {
        println(idDashCount + "\t" + extraUnderscoreIdx + "\t" + "idRaw: " + idRaw)
      }
       */

/*
      if ((idDashCount > 1) || (extraUnderscoreIdx >= 0)) {
        hasMultiple = true
*/
        val result = for {
          Some(M(map)) <- List(JSON.parseFull(line))
          S(id) = map("id")
          S(rowText) = map("context")
          D(label) = map("label")
          D(labelPred) = map("label_predicted")
          L(labelLogits) = map("label_logits")
          L(labelProbs) = map("label_probs")
        } yield {
          (id, rowText, label, labelPred, labelLogits, labelProbs)
        }

        val parsed = result(0)
        val lineID = parsed._1
        //println (fileIdx + "\t" + lineID)
        val elems = lineID.split("__")
        val rowUUID = elems.last
        val qidParts = elems.slice(0, elems.length-1).mkString("__").split("-")
        val questionID = qidParts.slice(0, qidParts.length - 1).mkString("-")

        if (idRaw.contains("MCAS_2006_9_9")) {
          println("questionID: " + questionID)
        }

        //if (qidParts.length > 2) hasMultiple = true
        //if (extraUnderscoreIdx >= 0) hasMultiple = true

        //val questionID = lineID.substring(0, lineID.indexOf("-"))
        //val rowUUID = lineID.substring(lineID.indexOf("__")+2, lineID.length)

        val rowText = parsed._2.substring(3, parsed._2.length)
        val label = parsed._3
        val labelPred = parsed._4
        val labelLogits = parsed._5
        val labelProbs = parsed._6

        val labelProb = labelProbs(1)
        val labelLogit = labelLogits(1)

        if (curQID != questionID) {
          println ("Saving... (" + curQID + " \t " + questionID + ")")
          // Save
/*
          if (hasMultiple == true) {
 */
            if (out.length > 0) {

              val sorted = out.sortBy(-_.getProbCorrect())
              val filenameOut = outPath + "/" + curQID + ".tsv"
              synchronized {
                count += 1
              }
              println(fileIdx + "\t" + count + "\t" + lineCount + "\t" + "Writing " + filenameOut)

              val pw = new PrintWriter(filenameOut)
              for (sLine <- sorted) {
                pw.println(sLine.toTSVLine())
              }

              pw.flush()
              pw.close()

            } else {
              println ("ERROR (" + curQID + " / " + questionID + "): out.length is zero. ")
            }
/*
          } else {
            println(fileIdx + "\t" + count + "\t" + curQID)
            synchronized {
              count += 1
            }
          }
*/
          // Clear
          out.clear()
          hasMultiple = false

          curQID = questionID
        }

        val ol = new OyvindRobertaOutLine(questionID, rowUUID, rowText, label, labelPred, labelLogits.toArray, labelProbs.toArray)
        out.append(ol)
//      }

      lineCount += 1
    }

    // Save last set
    val sorted = out.sortBy(-_.getProbCorrect())
    val filenameOut = outPath + "/" + curQID + ".tsv"
    synchronized {
      count += 1
    }
    println(fileIdx + "\t" + count + "\t" + lineCount + "\t" + "Writing " + filenameOut)

    val pw = new PrintWriter(filenameOut)
    for (sLine <- sorted) {
      pw.println(sLine.toTSVLine())
    }

    pw.flush()
    pw.close()



  }



  def main(args:Array[String]): Unit = {
    val path = "/home/peter/github/oyvind-allennlp6/eval-output-dir/"
    //val filenamePrefix = "eval-output-questionschunk"
    //val filenameSuffix = "-100neg"
    val filenamePrefix = "eval-dev-errortest5-trainnew-devnodupfiltered-5xneg"

    val outPath = "/home/peter/github/worldtree/explregen/oyvind-roberta-peterrun6-5x100neg/"


    val numFiles = 8
    val fileIdxs = Range(0, numFiles+1).par

    val numOfThreads = 14
    fileIdxs.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(numOfThreads))

    loadLines(path + filenamePrefix + ".jsonl", outPath, 0)

    /*
    for (fileIdx <- fileIdxs) {
      loadLines(path + filenamePrefix + fileIdx + filenameSuffix + ".jsonl", outPath, fileIdx)
    }
     */



  }

}
