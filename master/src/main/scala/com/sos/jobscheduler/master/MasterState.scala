package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.event.{Event, EventId, JournaledState, KeyedEvent}
import com.sos.jobscheduler.data.master.MasterFileBaseds.MasterTypedPathCompanions
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.data.agent.AgentSnapshot
import monix.reactive.Observable
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class MasterState(
  eventId: EventId,
  masterMetaState: MasterMetaState,
  repo: Repo,
  agents: Seq[AgentSnapshot],
  orders: Seq[Order[Order.State]])
extends JournaledState[MasterState, Event]
{
  protected type Self = MasterState
  protected type Evt = Event

  def toSnapshotObservable: Observable[Any] =
    Observable(masterMetaState) ++
    Observable.fromIterable(repo.eventsFor(MasterTypedPathCompanions)) ++
    Observable.fromIterable(agents) ++
    Observable.fromIterable(orders)

  def applyEvent: PartialFunction[KeyedEvent[_ <: Event], Checked[MasterState]] = ???

  def withEventId(eventId: EventId) =
    copy(eventId = eventId)
}

object MasterState
{
  def fromIterator(eventId: EventId, snapshotObjects: Iterator[Any]): MasterState = {
    val builder = new MasterStateBuilder
    snapshotObjects foreach builder.addSnapshot.orElse { case _: JournalHeader => }
    builder.state(eventId = eventId)
  }
}
