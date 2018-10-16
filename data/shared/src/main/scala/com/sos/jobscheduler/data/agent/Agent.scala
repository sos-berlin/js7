package com.sos.jobscheduler.data.agent

import com.sos.jobscheduler.base.utils.ScalazStyle._
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

  def withId(id: FileBasedId[AgentPath]) = copy(id = id)
}

object Agent extends FileBased.Companion[Agent]
{
  type ThisFileBased = Agent
  type Path = AgentPath

  implicit val jsonEncoder: ObjectEncoder[Agent] = agent ⇒
    JsonObject(
      "id" → (!agent.id.isAnonymous ? agent.id).asJson,
      "uri" → Json.fromString(agent.uri))

  implicit val jsonDecoder: Decoder[Agent] =
    cursor ⇒ for {
      id ← cursor.get[Option[AgentId]]("id") map (_ getOrElse AgentPath.NoId)
      uri ← cursor.get[String]("uri")
    } yield Agent(id, uri)

  override implicit val self = this
  implicit val fileBasedsOverview = AgentsOverview
  val typedPathCompanion = AgentPath
}
