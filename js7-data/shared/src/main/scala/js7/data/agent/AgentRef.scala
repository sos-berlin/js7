package js7.data.agent

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.{ItemRevision, SimpleItem}

final case class AgentRef(
  id: AgentId,
  uri: Uri,
  itemRevision: Option[ItemRevision] = None)
extends SimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = Some(revision))
}

object AgentRef extends SimpleItem.Companion[AgentRef]
{
  type Id = AgentId

  val cls = classOf[AgentRef]
  val Id = AgentId

  implicit val jsonCodec = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[AgentRef]
  }
}
