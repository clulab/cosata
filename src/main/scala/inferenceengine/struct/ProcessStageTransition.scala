package inferenceengine.struct

import inferenceengine.iml.model.StatementProc

/**
  * Created by user on 9/13/18.
  */
//case class ProcessStageTransition(processName:String, currentStage:String, transitionToStage:String) extends StatementProc {
case class ProcessStageTransition(transitionToStage:String) extends StatementProc {

  override def toString():String = {
    val os = new StringBuilder

    //os.append("ProcessStageTransition(ProcessName: " + processName + ", CurrentStage: " + currentStage + ", TransitionToStage: " + transitionToStage + ")")
    os.append("ProcessStageTransition(TransitionToStage: " + transitionToStage + ")")

    // Return
    os.toString()
  }
}



//case class ProcessStageComplete(processName:String, currentStage:String) extends StatementProc {
case class ProcessStageComplete() extends StatementProc {

  override def toString():String = {
    val os = new StringBuilder

    //os.append("ProcessStageComplete(ProcessName: " + processName + ", CurrentStage: " + currentStage + ")")
    os.append("ProcessStageComplete()")

    // Return
    os.toString()
  }
}