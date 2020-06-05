package js7.data.agent

import js7.base.circeutils.CirceUtils._
import js7.base.utils.ScalaUtils.reuseIfEqual
import js7.base.web.Uri
import js7.data.filebased.{FileBased, FileBasedId}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}

/**
  * @author Joacim Zschimmer
  */
final case class AgentRef(id: AgentRefId, uri: Uri) extends FileBased
{
  type Self = AgentRef

  val companion = AgentRef

  def withId(id: FileBasedId[AgentRefPath]) = reuseIfEqual(this, copy(id = id))
}

object AgentRef extends FileBased.Companion[AgentRef]
{
  type ThisFileBased = AgentRef
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
  implicit val fileBasedsOverview = AgentsOverview
  val typedPathCompanion = AgentRefPath
}
