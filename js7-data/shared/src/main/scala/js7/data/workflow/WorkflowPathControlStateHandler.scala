package js7.data.workflow

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._

trait WorkflowPathControlStateHandler[Self]
{
  this: Self =>

  protected def pathToWorkflowPathControlState: PartialFunction[WorkflowPath, WorkflowPathControlState]

  protected def updateWorkflowPathControlState(state: WorkflowPathControlState): Self

  protected final def applyWorkflowPathControlEvent(path: WorkflowPath, event: WorkflowPathControlEvent)
  : Checked[Self] =
    pathToWorkflowPathControlState
      .getOrElse(path, WorkflowPathControlState(WorkflowPathControl(path)))
      .applyEvent(event)
      .map(updateWorkflowPathControlState)
}
