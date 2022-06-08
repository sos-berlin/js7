package js7.data.workflow

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._

trait WorkflowControlStateHandler[Self]
{
  this: Self =>

  protected def pathToWorkflowControlState: PartialFunction[WorkflowPath, WorkflowControlState]

  protected def updateWorkflowControlState(state: WorkflowControlState): Self

  protected final def applyWorkflowControlEvent(path: WorkflowPath, event: WorkflowControlEvent)
  : Checked[Self] =
    pathToWorkflowControlState
      .getOrElse(path, WorkflowControlState(WorkflowControl(path)))
      .applyEvent(event)
      .map(updateWorkflowControlState)
}
