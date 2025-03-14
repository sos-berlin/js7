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
extends VersionedControl, UnsignedItemState, TrivialItemState[WorkflowControl]:

  val companion: WorkflowControl.type = WorkflowControl

  def workflowId: WorkflowId =
    id.workflowId

  def withRevision(revision: Option[ItemRevision]): WorkflowControl =
    copy(itemRevision = revision)

  // Duplicate in SimpleItem
  def nextRevision: WorkflowControl =
    withRevision:
      Some(itemRevision.fold(ItemRevision.Initial)(_.next))

  def workflowPath: WorkflowPath = id.path.workflowPath


object WorkflowControl extends
  VersionedControl.Companion[WorkflowControl],
  UnsignedItemState.Companion[WorkflowControl],
  TrivialItemState.Companion[WorkflowControl]:

  type Item = WorkflowControl
  val cls: Class[WorkflowControl] = classOf[WorkflowControl]

  override type Key = WorkflowControlId

  type Path = WorkflowControlPath
  val Path: WorkflowControlPath.type = WorkflowControlPath

  implicit val jsonCodec: Codec.AsObject[WorkflowControl] =
    deriveCodec[WorkflowControl]
