package js7.data.workflow

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.item.{ItemRevision, TrivialItemState, UnsignedItemState, VersionedControl}
import js7.data.workflow.WorkflowControlId.syntax.*
import js7.data.workflow.position.Position

final case class WorkflowControl(
  id: WorkflowControlId,
  breakpoints: Set[Position] = Set.empty,
  itemRevision: Option[ItemRevision] = None)
extends VersionedControl
with UnsignedItemState
with TrivialItemState[WorkflowControl]
{
  protected type Self = WorkflowControl
  val companion: WorkflowControl.type = WorkflowControl
  val item: WorkflowControl = this

  def workflowId: WorkflowId =
    id.workflowId

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def workflowPath = id.path.workflowPath
}

object WorkflowControl
extends VersionedControl.Companion[WorkflowControl]
with UnsignedItemState.Companion[WorkflowControl]
{
  type Item = WorkflowControl
  val cls = classOf[WorkflowControl]

  override type Key = WorkflowControlId

  type Path = WorkflowControlPath
  val Path = WorkflowControlPath

  override type ItemState = WorkflowControl

  implicit val jsonCodec: Codec.AsObject[WorkflowControl] =
    deriveCodec[WorkflowControl]
}
