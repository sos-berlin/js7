package js7.data.workflow

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Codec, Decoder}
import js7.data.item.ItemRevision
import js7.data.workflow.position.Label

final case class WorkflowPathControl(
  path: WorkflowPath,
  suspended: Boolean = false,
  skip: Set[Label] = Set.empty,
  revision: ItemRevision = ItemRevision(0))

object WorkflowPathControl
{
  private val jsonDecoder: Decoder[WorkflowPathControl] =
    c => for {
      path <- c.get[WorkflowPath]("path")
      suspended <- c.get[Boolean]("suspended")
      skip <- c.getOrElse[Set[Label]]("skip")(Set.empty)
      revision <- c.get[ItemRevision]("revision")
    } yield WorkflowPathControl(path, suspended, skip, revision)

  implicit val jsonCodec = Codec.from(jsonDecoder, deriveEncoder[WorkflowPathControl])
}
