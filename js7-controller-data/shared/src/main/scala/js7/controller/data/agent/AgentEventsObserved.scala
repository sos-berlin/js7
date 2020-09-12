package js7.controller.data.agent

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.agent.AgentRefPath
import js7.data.event.{Event, EventId}

/**
  * @author Joacim Zschimmer
  */
final case class AgentEventsObserved(untilEventId: EventId) extends Event
{
  type Key = AgentRefPath

  override def toString = s"AgentEventsObserved(${EventId.toString(untilEventId)})"
}

object AgentEventsObserved
{
  implicit val jsonCodec = TypedJsonCodec[AgentEventsObserved](
    Subtype(deriveCodec[AgentEventsObserved]))
}
