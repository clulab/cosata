package inferenceengineexperiments.preprocessing

import data.question.ExamQuestionParserDynamic
import data.question.ExamQuestionUtils.convertToExplQuestions

import scala.collection.mutable

object CheckForDuplicates {

  def main(args:Array[String]): Unit = {
    val filenameTrain = "annotation/expl-tablestore-export-2020-02-17-123232/questions.train.tsv.nodup.filtered.tsv"
    val filenameEval = "annotation/expl-tablestore-export-2020-02-17-123232/questions.dev.tsv.nodup.filtered.tsv"

    var questionsInTrain = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameTrain, fullAnnotation = false, noAnnotation = false, debugQuestionLimit = 0, tsvMode = true)
    var explQuestionsInTrain = convertToExplQuestions(questionsInTrain)

    var questionsInEval = ExamQuestionParserDynamic.loadQuestionsFromCSVList(filenameEval, fullAnnotation = false, noAnnotation = false, debugQuestionLimit = 0, tsvMode = true)
    var explQuestionsInEval = convertToExplQuestions(questionsInEval)

    val qIDsTrain = mutable.Set[String]()
    val qIDsEval = mutable.Set[String]()

    for (q <- explQuestionsInTrain) {
      val qid = q.question.questionID.split("_ENUM")(0)
      qIDsTrain.add(qid)
    }

    for (q <- explQuestionsInEval) {
      val qid = q.question.questionID.split("_ENUM")(0)
      qIDsEval.add(qid)
    }

    println ("Number of unique question IDs (train): " + qIDsTrain.size)
    println ("Number of unique question IDs (eval): " + qIDsEval.size)

    val intersection = qIDsTrain.intersect(qIDsEval)

    println ("Number of leaked questions in evaluation set: " + intersection.size)
    println ("Intersection: " + intersection.mkString(", "))


  }

}
