package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class Agent(id: AgentId, uri: String) extends FileBased
{
  type Self = Agent

  val companion = Agent

  def withId(id: FileBasedId[AgentPath]) = reuseIfEqual(this, copy(id = id))
}

object Agent extends FileBased.Companion[Agent]
{
  type ThisFileBased = Agent
  type Path = AgentPath

  implicit val jsonEncoder: ObjectEncoder[Agent] = agent ⇒
    agent.id.asJsonObject ++
      JsonObject(
        "uri" → Json.fromString(agent.uri))

  implicit val jsonDecoder: Decoder[Agent] =
    cursor ⇒ for {
      id ← cursor.as[Option[AgentId]] map (_ getOrElse AgentPath.NoId)
      uri ← cursor.get[String]("uri")
    } yield Agent(id, uri)

  override implicit val self = this
  implicit val fileBasedsOverview = AgentsOverview
  val typedPathCompanion = AgentPath
}
