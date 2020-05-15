package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.exceptions.HasIsIgnorableStackTrace
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.data.agent.AgentRef
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent
import com.sos.jobscheduler.data.order.{Order, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
trait MasterApi extends SessionApi.HasUserAndPassword with HasIsIgnorableStackTrace
{
  def executeCommand(command: MasterCommand): Task[command.Response]

  def overview: Task[MasterOverview]

  def clusterState: Task[ClusterState]

  def events[E <: Event: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def fatEvents[E <: OrderFatEvent: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[Observable[Stamped[KeyedEvent[E]]]]

  def eventIdObservable[E <: Event: ClassTag](request: EventRequest[E], heartbeat: Option[FiniteDuration] = None)
    : Task[Observable[EventId]]

  def journalObservable(fileEventId: EventId, position: Long,
    heartbeat: Option[FiniteDuration] = None, timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false, returnLength: Boolean = false)
  : Task[Observable[ByteVector]]

  def ordersOverview: Task[OrdersOverview]

  def orders: Task[Checked[Seq[Order[Order.State]]]]

  def workflows: Task[Checked[Seq[Workflow]]]

  def agents: Task[Checked[Seq[AgentRef]]]

  def snapshot: Task[Checked[Stamped[Seq[Any]]]]
}
