package com.sos.jobscheduler.master

import cats.syntax.option.none
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.Collections.implicits.InsertableMutableMap
import com.sos.jobscheduler.common.event.EventBasedState
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.AgentRefId
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.master.MasterFileBaseds._
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.master.agent.AgentEventId
import com.sos.jobscheduler.master.scheduledorder.OrderScheduleEndedAt
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  repo: Repo,
  idToOrder: Map[OrderId, Order[Order.State]],
  agentToEventId: Map[AgentRefId, EventId],
  orderScheduleEndedAt: Option[Timestamp])
extends EventBasedState
{
  def toSnapshots: Seq[Any] =
    repo.eventsFor(MasterTypedPathCompanions) ++
    agentToEventId.toVector.map(o ⇒ AgentEventId(o._1, o._2)) ++
    idToOrder.values
}

object MasterState
{
  def fromIterable(eventId: EventId, snapshotObjects: Iterator[Any]): MasterState = {
    var repo = Repo(MasterFileBaseds.jsonCodec)
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    val agentToEventId = mutable.Map[AgentRefId, EventId]()
    var orderScheduleEndedAt = none[Timestamp]

    snapshotObjects foreach {
      case order: Order[Order.State] ⇒
        idToOrder.insert(order.id → order)

      case AgentEventId(agentRefPath, aEventId) ⇒
        agentToEventId(agentRefPath) = aEventId

      case event: RepoEvent ⇒
        repo = repo.applyEvent(event).orThrow

      case OrderScheduleEndedAt(timestamp) ⇒
        orderScheduleEndedAt = Some(timestamp)
    }
    MasterState(eventId, repo, idToOrder.toMap, agentToEventId.toMap, orderScheduleEndedAt)
  }
}
