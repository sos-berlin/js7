package js7.data.agent

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils._
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.base.web.Uri
import js7.data.item.{InventoryItem, ItemId}

/**
  * @author Joacim Zschimmer
  */
final case class AgentRef(id: AgentRefId, uri: Uri) extends InventoryItem
{
  type Self = AgentRef

  val companion = AgentRef

  def withId(id: ItemId[AgentRefPath]) = reuseIfEqual(this, copy(id = id))
}

object AgentRef extends InventoryItem.Companion[AgentRef]
{
  type ThisItem = AgentRef
  type Path = AgentRefPath

  implicit val jsonEncoder: Encoder.AsObject[AgentRef] = agent =>
    agent.id.asJsonObject ++
      JsonObject(
        "uri" -> Json.fromString(agent.uri.string))

  implicit val jsonDecoder: Decoder[AgentRef] =
    cursor => for {
      id <- cursor.as[Option[AgentRefId]] map (_ getOrElse AgentRefPath.NoId)
      uri <- cursor.get[Uri]("uri")
    } yield AgentRef(id, uri)

  override implicit val self = this
  implicit val itemsOverview = AgentsOverview
  val typedPathCompanion = AgentRefPath
}
