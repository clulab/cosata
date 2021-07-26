package util

import edu.arizona.sista.struct.{Counter, Lexicon}

import scala.collection.mutable.ArrayBuffer

/**
 * Basic Confusion Matrix
 * Created by peter on 3/4/16.
 */
class ConfusionMatrix {
  val lexicon = new Lexicon[String]
  val confusionMatrix = new ArrayBuffer[Counter[String]]()

  /*
   * Main usage method
   */

  def addData(labelActual:String, labelPredicted:String) {
    val labelActualIdx = lexicon.add(labelActual)
    val labelPredictedIdx = lexicon.add(labelPredicted)

    // Expand, if required
    expand(labelActualIdx)
    expand(labelPredictedIdx)

    // Add
    confusionMatrix(labelActualIdx).incrementCount(labelPredicted, 1.0)
  }


  /*
   * String Output
   */
  def mkMatrixString(optionalOrder:Array[String] = Array.empty[String], maxLabelLength:Int = 12, sortLabels:Boolean = false, delim:String = ""): String = {
    val os = new StringBuilder

    // Step 1: Determine label order
    var labelOrder = lexicon.keySet.toArray
    if (optionalOrder.size > 0) {
      labelOrder = optionalOrder
    }
    if (sortLabels) {
      labelOrder = labelOrder.sorted
    }

    val numLabels:Int = labelOrder.size

    // Step 1A: Automatically size maxLabelLength if a zero is specified
    var maxLabelLength1 = maxLabelLength
    if (maxLabelLength == 0) {
      for (label <- lexicon.keySet) {
        if (label.length > maxLabelLength1) maxLabelLength1 = label.length
      }
      maxLabelLength1 += 2
      maxLabelLength1 = math.max(maxLabelLength1, 12)
    }

    // Step 2: Header
    os.append( constLen(" actual/pred", maxLabelLength1 + 2) + delim )     // Lead in
    for (label <- labelOrder) {
      os.append( constLen(label, maxLabelLength1, hardLimit = true) + delim )
    }
    os.append("SAMPLES   " + delim)
    os.append("TPR       " + delim)
    os.append("TRUE POSITIVE RATE" + delim)
    os.append("\n")

    val falsePos = Array.fill[Double](numLabels)(0)
    val truePos = Array.fill[Double](numLabels)(0)

    // Step 3: Table
    // Actual label
    for (i <- 0 until labelOrder.size) {
      val labelActual = labelOrder(i)
      os.append( constLen(labelActual, maxLabelLength1, hardLimit = true) + "  " + delim)
      val counterActual = confusionMatrix( lexicon.get(labelActual).get )

      // Predicted label
      var correct:Double = 0.0
      var incorrect:Double = 0.0
      for (j <- 0 until labelOrder.size) {
        val labelPredicted = labelOrder(j)
        val value = counterActual.getCount(labelPredicted)
        os.append( constLen(value.formatted("%1.0f"), maxLabelLength1, hardLimit = true) + delim)

        if (labelPredicted == labelActual) {
          correct += value
          truePos(j) += value
        } else {
          incorrect += value
          falsePos(j) += value
        }
      }

      // Report in-class correctness
      os.append ( constLen( (correct + incorrect).toString, 10) + delim)
      os.append ( constLen( (correct / (correct + incorrect)).formatted("%3.3f"), 10) + delim)
      os.append ("  ")
      os.append ("(" + correct.formatted("%1.0f") + " correct, " + incorrect.formatted("%1.0f") + " errors) = " + (100 * correct / (correct + incorrect)).formatted("%3.1f") + "% correct")
      os.append("\n")
    }

    os.append("---\n")
    os.append(constLen("FALSE POS", maxLabelLength1, hardLimit = true) + delim)
    os.append("  ")
    for (i <- 0 until labelOrder.size) {
      val fpRate = 100 * falsePos(i) / (truePos(i) + falsePos(i))
      os.append( constLen(fpRate.formatted("%3.1f") + "%", maxLabelLength1, hardLimit = true) + delim)
    }
    os.append("\n")


    // Return
    os.toString
  }



  /*
   Supporting Methods
   */
  def expand(size:Int): Unit = {
    val diff = (size+1) - confusionMatrix.size
    for (i <- 0 until diff) {
      confusionMatrix.append(new Counter[String])
    }
  }

  def constLen(in:String, length:Int, hardLimit:Boolean = false):String = {
    var os = new StringBuilder
    os.append(in)
    for (i <- 0 until (length - in.size)) {
      os.append(" ")
    }
    // Hard limit clipping
    if ((hardLimit) && (os.size > length)) {
      os.replace(length-3, os.length, "...")
    }

    os.toString
  }

}

object ConfusionMatrix {

  def main(args: Array[String]) {
    val cm = new ConfusionMatrix

    val labels = Array("banana", "orange", "watermelon")
    for (i <- 0 until 1000) {
      val randLabelActual = labels(scala.util.Random.nextInt(labels.size))

      val prediction = math.round(scala.util.Random.nextInt((labels.size*2)-1).toFloat / 2)
      val randLabelPred = labels(prediction)

      cm.addData(randLabelActual, randLabelPred)
    }


    println ( cm.mkMatrixString(labels) )


  }

}
