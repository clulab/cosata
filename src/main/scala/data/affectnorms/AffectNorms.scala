package data.affectnorms

import edu.arizona.sista.struct.Counter
import org.slf4j.LoggerFactory

/**
 * Wrapper for Affect norms database, "Norms of valence, arousal, and dominance for 13,915 English lemmas", by
 * Warriner, Kuperman, and Brysbaert, Behavior Research Methods (2013)
 *
 * Original ratings:
 * Valience: Unhappy[1] to Happy[9]
 * Arousal: Calm[1] to Excited [9]
 * Dominance: Controlled[1] to In-Control [9]
 *
 * Ratings returned here are rescaled to between 0 (1) and 1 (9).
 *
 * Created by peter on 2/19/16.
 */
class AffectNorms {

}

object AffectNorms {

  val logger = LoggerFactory.getLogger(classOf[AffectNorms])
  val FILENAME_DEFAULT_AFFECTNORMS = "db/affect/Affective_Ratings_Warriner_et_al.csv"

  val valience = new Counter[String](defaultReturnValue = -1.0)
  val arousal = new Counter[String](defaultReturnValue = -1.0)
  val dominance = new Counter[String](defaultReturnValue = -1.0)


  def getValience(lemma:String):Double = {
    valience.getCount(lemma)
  }

  def getArousal(lemma:String):Double = {
    arousal.getCount(lemma)
  }

  def getDominance(lemma:String):Double = {
    dominance.getCount(lemma)
  }


  def loadAffectNormsDatabase(filename:String) {
    logger.info (" * Loading Affect norms database (" + filename + ")...")
    val out = new Counter[String]

    var lineCount:Int = 0
    for (line <- io.Source.fromFile(filename, "UTF-8").getLines()) {
      if (lineCount > 0) {
        // Skip header on first line
        val split = line.toLowerCase.split(",")
        if (split.size > 9) {
          val word = split(1).trim.toLowerCase

          val valienceRating = parseDouble(split(2).trim())
          val arousalRating = parseDouble(split(5).trim())
          val dominanceRating = parseDouble(split(8).trim())

          if (valienceRating.isDefined) valience.setCount(word, ((valienceRating.get - 1) / 8.0))
          if (arousalRating.isDefined) arousal.setCount(word, ((arousalRating.get - 1) / 8.0))
          if (dominanceRating.isDefined) dominance.setCount(word, ((dominanceRating.get - 1) / 8.0))
        }
      }
      lineCount += 1
    }

    logger.info (" * Affect norms loaded.  (" + valience.keySet.size + " lemmas)" )
  }

  def parseDouble(in:String):Option[Double] = {
    try{
      Some(in.toDouble)
    } catch {
      case _:Throwable => None
    }
  }


  /*
   * Example of use
   */
  def main(args: Array[String]): Unit = {
    AffectNorms.loadAffectNormsDatabase(AffectNorms.FILENAME_DEFAULT_AFFECTNORMS)

    var word:String = "dog"
    println ("Valience of " + word + " = " + AffectNorms.getValience(word) )

    println ("Arousal of " + word + " = " + AffectNorms.getArousal(word) )

    println ("Dominance of " + word + " = " + AffectNorms.getDominance(word) )

  }

}
