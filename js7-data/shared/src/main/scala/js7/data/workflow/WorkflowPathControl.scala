package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.data.item.ItemRevision

final case class WorkflowPathControl(
  path: WorkflowPath,
  suspended: Boolean = false,
  revision: ItemRevision = ItemRevision(0))

object WorkflowPathControl
{
  implicit val jsonCodec = deriveCodec[WorkflowPathControl]
}
