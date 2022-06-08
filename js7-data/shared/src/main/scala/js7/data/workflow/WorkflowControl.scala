package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.data.item.ItemRevision

final case class WorkflowControl(
  path: WorkflowPath,
  suspended: Boolean = false,
  revision: ItemRevision = ItemRevision(0))

object WorkflowControl
{
  implicit val jsonCodec = deriveCodec[WorkflowControl]
}
