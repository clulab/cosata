package data.question

import java.io.File

import collection.mutable.ArrayBuffer
import edu.arizona.sista.utils.StringUtils
import org.slf4j.LoggerFactory
import ExamQuestionParser.logger
import edu.arizona.sista.processors.{Processor, Document}
import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import com.github.tototoshi.csv._


/**
 * Parser for exam questions in AI2 format
 * User: peter
 * Date: 1/13/14
 */
class ExamQuestionParser {

}

object ExamQuestionParser {
  lazy val processor:Processor = new CoreNLPProcessor()
  val logger = LoggerFactory.getLogger(classOf[ExamQuestionParser])

  // Main entry point
  def loadQuestionsFromCSVList(qList:String, fullAnnotation:Boolean = true):Array[MCQuestion] = {
    toMCQuestion( parse( qList.split(",").toArray ), fullAnnotation )
  }

  def parse(filenames:Array[String]):Array[MCQuestionRaw] = {
    // Returns all multiple choice questions from the list of files supplied ("filenames")

    // Step 1: Load all questions from supplied files
    val questions = new ArrayBuffer[ExamQuestion]
    for (filename <- filenames) {
      questions ++= load(filename)
    }

    // Step 2: Filter to only include MC questions that do not contain diagrams
    var i:Int = 0
    while (i < questions.size) {
      if ((questions(i).isMultipleChoice) && (!questions(i).hasDiagram)) {
        i += 1
      } else {
        questions.remove(i)
      }
    }

    // Step 3: Cast to generic MCQuestionRaw type
    val questionsMC = new Array[MCQuestionRaw](questions.size)
    for (i <- 0 until questions.size) {
      questionsMC(i) = questions(i).toMCQuestion
    }

    // Step 4: Display questions
    for (question <- questionsMC) {
      logger.debug (question.toString)
      logger.debug ("----")
    }
    logger.info ("Parsed " + questionsMC.size + " questions after filtering. ")

    questionsMC
  }

  // Parses MC Questions in plain text format. e.g. What colour is water? (a) red (b) green (c) blue (d) orange
  // Returns tuple of (question text, Array[Answer Text])
  def textToMC(text:String):(String, Array[String]) = {
    val qsplit = text.split("\\(([A-D]|[a-d]|[1-4])\\)")
    val questionText = qsplit(0).trim
    val mcAnswers = new ArrayBuffer[String]
    for (i <- 1 until qsplit.size) {
      mcAnswers.append(qsplit(i).trim)
    }
    (questionText, mcAnswers.toArray)
  }

  // Convert from a ExamQuestion back to the AI2 Question file format
  def MCToText(in:ExamQuestion):String = {
    val os = new StringBuilder
    os.append(in.qID + "\t")
    os.append(in.originalID + "\t")
    os.append(in.totalPoints + "\t")
    os.append((65 + in.correctAnswer).toChar + "\t")

    if (in.isMultipleChoice) {
      os.append("1\t")
    } else {
      os.append("0\t")
    }
    if (in.hasDiagram) {
      os.append("1\t")
    } else {
      os.append("0\t")
    }

    os.append(in.examName + "\t")
    os.append(in.schoolGrade + "\t")
    os.append(in.year + "\t")
    os.append(in.questionType + "\t")
    os.append(in.text + "    ")

    for (aIdx <- 0 until in.choices.size) {
      val letter:Char = (65 + aIdx).toChar
      os.append(" (" + letter + ") " + in.choices(aIdx))
    }

    os.toString()
  }

  // Convert from letter indicies of answer candidates (A, B, C, D) to numeric Int indicies (0, 1, 2, 3)
  def letterAnswerToNumericIndex(in:String):Int = {
    if (in.size != 1) {
      return -1
    }

    // Normal case
    if (in.matches("[A-D]")) {
      return in.charAt(0) - 65
    } else if (in.matches("[1-4]")) {
      return in.toInt - 1
    }

    // Error
    -1
  }

  // TODO: This function should likely be rewritten to read the header, and dynamically adjust which fields it populates based on which fields are populated in the file it reads.
  def load(filename:String):Array[ExamQuestion] = {
    val questions = new ArrayBuffer[ExamQuestion]()
    println ("Filename:" + filename)

    val reader = CSVReader.open(new File(filename))
    var lines = reader.all()

    // Strip header
    lines = lines.slice(1, lines.size)

    // Check to see if we have 12 fields (normal) or 13 fields (inference type annotated)
    var inferenceTypeMod:Int = 0
    var goldJustificationMod:Int = 0
    if (lines(0).size >= 14) inferenceTypeMod = 1
    if (lines(0).size >= 15) goldJustificationMod = 1

    for (fields <- lines) {
      logger.debug( fields.toList.toString() )
      logger.debug(" fields.size = " + fields.size)

      val qID = fields(0)
      val originalID = fields(1)
      var totalPoints = 1   // default
      if (fields(2).length > 0) totalPoints = fields(2).toInt     // Occasionally unspecified
      var answerKey:Int = -1                    // May be either NULL (-1), integer 1-4, or character A-D
      // Reindex from 1-4/A-D to 0-3 (to match MC array indicies)
      answerKey = letterAnswerToNumericIndex( fields(3) )
      /*
      if ((fields(3).size == 1) && (fields(3).matches("[A-D]"))) {
        answerKey = fields(3).charAt(0) - 65
      } else if ((fields(3).size == 1) && (fields(3).matches("[1-4]"))) {
        answerKey = fields(3).toInt - 1
      }
      */

      var isMultipleChoice:Boolean = false
      if (fields(4) == "1") isMultipleChoice = true
      var hasDiagram:Boolean = false
      if (fields(5) == "1") hasDiagram = true
      val examName = fields(6)
      val schoolGrade = fields(7)
      val year = fields(8)

      // Inference type (may or may not be present)
      var questionType = ""
      if (inferenceTypeMod != 0) questionType = fields(9).trim()
      // Check for/remove a '?' at the end of the question type
      println (questionType + " (" + questionType.size + ")")
      if ((questionType.size > 1) && (questionType(questionType.size-1) == '?')) {
        questionType = questionType.substring(0, questionType.size - 1)
      }

      // Question Topic(s)/Topic Cluster
      var topics = fields(10).toUpperCase.trim()

      var goldJustification = ""
      if (goldJustificationMod != 0) goldJustification = fields(11).trim()


      var fieldOffset = inferenceTypeMod + goldJustificationMod
      var question = fields(10 + fieldOffset).trim()
      println ("questionidx: " + (10 + fieldOffset).toInt)

      // Check for/remove quotes encasing the question text
      if ((question(0) == '\"') && (question(question.size-1) == '\"')) {
        question = question.substring(1, question.size-1)
      }


      //### DEBUG
      //if (questionType.startsWith("CAUSAL") || questionType.startsWith("PROCESS")) questionType = "CAUSE/PROC"

      // Extract multiple choice answer candidates from multiple choice questions
      if (isMultipleChoice) {
        val (questionText, mcAnswers) = textToMC(question)

        // Store Regents MC Question
        val rq = new ExamQuestion(qID, originalID, totalPoints, answerKey, isMultipleChoice,
          hasDiagram, examName, schoolGrade, year, questionType, topics, fold = "", category = "", goldJustification, questionText, mcAnswers)
        questions.append(rq)

      } else {
        val rq = new ExamQuestion(qID, originalID, totalPoints, answerKey, isMultipleChoice,
          hasDiagram, examName, schoolGrade, year, questionType, topics, fold = "", category = "", goldJustification, question, Array.empty[String])
        questions.append(rq)
      }

    }

    questions.toArray
  }

  def toMCQuestion(in:Array[MCQuestionRaw], fullAnnotation:Boolean = true):Array[MCQuestion] = {
    val out = new Array[MCQuestion](in.size)

    for (i <- 0 until in.size) {
      val question = in(i)
      val choices = new Array[MCAnswer](question.choices.size)

      // Annotate multiple choice answers
      for (j <- 0 until choices.size) {
        val aText = question.choices(j)
        //val answer = new MCAnswer(aText, mkPartialAnnotation(aText))
        var answer:MCAnswer = null
        if (fullAnnotation) {
          answer = new MCAnswer(aText, processor.annotate(aText), Array.empty[String])
        } else {
          answer = new MCAnswer(aText, mkPartialAnnotation(aText), Array.empty[String])
        }
        choices(j) = answer
      }
      // Annotate question
      //val qAnnotation = mkPartialAnnotation(question.text)
      var qAnnotation:Document = null
      var jAnnotation:Document = null
      if (fullAnnotation) {
        qAnnotation = processor.annotate( question.text )
        jAnnotation = processor.annotate( question.justification )
      } else {
        qAnnotation = mkPartialAnnotation( question.text )
        jAnnotation = mkPartialAnnotation( question.justification )
      }

      // Split topic(s)
      val topics = new ArrayBuffer[String]
      for (topic <- question.topics.split(",")) {
        topics.append( topic.toUpperCase.trim )
      }

      out(i) = new MCQuestion(question.text, qAnnotation, question.qType, question.justification, jAnnotation, question.grade, topics.toArray, fold = "", category = "", question.questionID, question.correctAnswer, choices)
    }

    out
  }

  def mkPartialAnnotation(text:String):Document = {
    val doc = processor.mkDocument(text)
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    doc.clear()
    doc
  }



  def main(args:Array[String]) {
    val props = StringUtils.argsToProperties(args)

    val path:String = "/data/nlp/corpora/AriResources/AI2-Elementary-Feb2016/"
    val filenames = new ArrayBuffer[String]
    filenames.append(path + "Elementary-NDMC-Train.csv")

    parse( filenames.toArray )

  }

}