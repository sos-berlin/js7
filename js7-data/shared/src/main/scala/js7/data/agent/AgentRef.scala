package js7.data.agent

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils._
import js7.base.web.Uri
import js7.data.item.SimpleItem

final case class AgentRef(id: AgentId, uri: Uri)
extends SimpleItem
{
  protected type Self = AgentRef
  val companion = AgentRef
}

object AgentRef extends SimpleItem.Companion
{
  type Item = AgentRef
  type Id = AgentId

  implicit val jsonCodec = deriveCodec[AgentRef]
  implicit val jsonEncoder: Encoder.AsObject[AgentRef] = jsonCodec
  implicit val jsonDecoder: Decoder[AgentRef] = jsonCodec
}
