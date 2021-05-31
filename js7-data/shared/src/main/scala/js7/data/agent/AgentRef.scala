package js7.data.agent

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.{ItemRevision, UnsignedSimpleItem}

final case class AgentRef(
  path: AgentPath,
  uri: Uri,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def referencedItemPaths = Set.empty
}

object AgentRef extends UnsignedSimpleItem.Companion[AgentRef]
{
  val cls = classOf[AgentRef]

  type Key = AgentPath
  val Key = AgentPath

  implicit val jsonCodec = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[AgentRef]
  }
}
