package js7.data.agent

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.{ItemRevision, SimpleItem}

final case class AgentRef(
  id: AgentId,
  uri: Uri,
  itemRevision: ItemRevision = ItemRevision.Initial)
extends SimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = revision)
}

object AgentRef extends SimpleItem.Companion
{
  type Item = AgentRef
  type Id = AgentId

  val cls = classOf[AgentRef]
  val idCompanion = AgentId

  implicit val jsonCodec: CirceObjectCodec[AgentRef] = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[AgentRef]
  }
}
