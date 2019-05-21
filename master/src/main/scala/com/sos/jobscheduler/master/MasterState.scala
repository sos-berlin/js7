package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.event.EventBasedState
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.master.MasterFileBaseds
import com.sos.jobscheduler.data.master.MasterFileBaseds.MasterTypedPathCompanions
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import monix.reactive.Observable
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  repo: Repo,
  agents: Seq[AgentSnapshot],
  orders: Seq[Order[Order.State]])
extends EventBasedState
{
  def toSnapshots: Seq[Any] =
    repo.eventsFor(MasterTypedPathCompanions) ++
      agents ++
      orders

  def toSnapshotObservable: Observable[Any] =
    Observable.fromIterable(repo.eventsFor(MasterTypedPathCompanions)) ++
    Observable.fromIterable(agents) ++
    Observable.fromIterable(orders)
}

object MasterState
{
  def fromIterable(eventId: EventId, snapshotObjects: Iterator[Any]): MasterState = {
    var repo = Repo(MasterFileBaseds.jsonCodec)
    val orders = Vector.newBuilder[Order[Order.State]]
    val agents = Vector.newBuilder[AgentSnapshot]

    snapshotObjects foreach {
      case order: Order[Order.State] =>
        orders += order

      case agent: AgentSnapshot =>
        agents += agent

      case event: RepoEvent =>
        repo = repo.applyEvent(event).orThrow
    }
    MasterState(eventId, repo, agents.result(), orders.result())
  }
}
