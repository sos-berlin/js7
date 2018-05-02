package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.agent.AgentEventId
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterTypedPathCompanions
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  repo: Repo,
  orders: Seq[Order[Order.State]],
  agentToEventId: Map[AgentId, EventId],
  orderScheduleEndedAt: Option[Timestamp])
{
  def toSnapshots: Seq[Any] =
    repo.eventsFor(MasterTypedPathCompanions) ++
    agentToEventId.toVector.map(o ⇒ AgentEventId(o._1, o._2)) ++
    orders
}
