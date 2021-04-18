package js7.data.agent

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.{ItemRevision, UnsignedSimpleItem}

final case class AgentRef(
  id: AgentId,
  uri: Uri,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object AgentRef extends UnsignedSimpleItem.Companion[AgentRef]
{
  type Id = AgentId

  val cls = classOf[AgentRef]
  val Id = AgentId

  implicit val jsonCodec = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[AgentRef]
  }
}
