package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.event.EventBasedState
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.master.MasterFileBaseds.MasterTypedPathCompanions
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.agent.AgentEventId
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  repo: Repo,
  orders: Seq[Order[Order.State]],
  agentToEventId: Map[AgentRefPath, EventId])
extends EventBasedState
{
  def toSnapshots: Seq[Any] =
    repo.eventsFor(MasterTypedPathCompanions) ++
    agentToEventId.toVector.map(o => AgentEventId(o._1, o._2)) ++
    orders

  def toSnapshotObservable: Observable[Any] =
    Observable.fromIterable(repo.eventsFor(MasterTypedPathCompanions)) ++
    Observable.fromIterable(agentToEventId).map(o => AgentEventId(o._1, o._2)) ++
    Observable.fromIterable(orders)
}

object MasterState
{
  def fromIterable(eventId: EventId, snapshotObjects: Iterator[Any]): MasterState = {
    var repo = Repo(MasterFileBaseds.jsonCodec)
    val orders = Vector.newBuilder[Order[Order.State]]
    val agentToEventId = mutable.Map[AgentRefPath, EventId]()

    snapshotObjects foreach {
      case order: Order[Order.State] =>
        orders += order

      case AgentEventId(agentRefPath, aEventId) =>
        agentToEventId(agentRefPath) = aEventId

      case event: RepoEvent =>
        repo = repo.applyEvent(event).orThrow
    }
    MasterState(eventId, repo, orders.result(), agentToEventId.toMap)
  }
}
