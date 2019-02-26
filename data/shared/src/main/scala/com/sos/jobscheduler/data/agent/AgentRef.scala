package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class AgentRef(id: AgentRefId, uri: String) extends FileBased
{
  type Self = AgentRef

  val companion = AgentRef

  def withId(id: FileBasedId[AgentRefPath]) = reuseIfEqual(this, copy(id = id))
}

object AgentRef extends FileBased.Companion[AgentRef]
{
  type ThisFileBased = AgentRef
  type Path = AgentRefPath

  implicit val jsonEncoder: ObjectEncoder[AgentRef] = agent =>
    agent.id.asJsonObject ++
      JsonObject(
        "uri" -> Json.fromString(agent.uri))

  implicit val jsonDecoder: Decoder[AgentRef] =
    cursor => for {
      id <- cursor.as[Option[AgentRefId]] map (_ getOrElse AgentRefPath.NoId)
      uri <- cursor.get[String]("uri")
    } yield AgentRef(id, uri)

  override implicit val self = this
  implicit val fileBasedsOverview = AgentsOverview
  val typedPathCompanion = AgentRefPath
}
