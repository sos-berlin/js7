package js7.data.workflow

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.item.{ItemRevision, TrivialItemState, UnsignedSimpleItem, UnsignedSimpleItemState}
import js7.data.workflow.position.Label

final case class WorkflowPathControl(
  override val path: WorkflowPathControlPath,
  suspended: Boolean = false,
  skip: Set[Label] = Set.empty,
  itemRevision: Option[ItemRevision] = Some(ItemRevision(0)))
extends UnsignedSimpleItem with TrivialItemState
{
  protected type Self = WorkflowPathControl
  val companion = WorkflowPathControl
  val item = this

  def toInitialItemState = this

  def rename(path: WorkflowPathControlPath) =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def workflowPath = path.workflowPath
}

object WorkflowPathControl
extends UnsignedSimpleItem.Companion[WorkflowPathControl]
with UnsignedSimpleItemState.Companion[WorkflowPathControl]
{
  type Key = WorkflowPathControlPath
  val Key = WorkflowPathControlPath

  val Path = WorkflowPathControlPath
  val cls = classOf[WorkflowPathControl]

  type ItemState = WorkflowPathControl

  implicit val jsonCodec: Codec.AsObject[WorkflowPathControl] =
    deriveCodec[WorkflowPathControl]
}
