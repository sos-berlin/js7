package js7.controller.client

import io.circe.{Decoder, Encoder}
import js7.base.problem.Checked
import js7.controller.data.{ControllerCommand, ControllerOverview}
import js7.data.agent.AgentRef
import js7.data.cluster.ClusterState
import js7.data.event.{Event, EventApi, EventId, EventRequest, KeyedEvent, TearableEventSeq}
import js7.data.fatevent.OrderFatEvent
import js7.data.order.{Order, OrdersOverview}
import js7.data.workflow.Workflow
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
trait ControllerApi
extends EventApi
{
  def executeCommand(command: ControllerCommand): Task[command.Response]

  def overview: Task[ControllerOverview]

  def clusterState: Task[ClusterState]

  def events[E <: Event: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def fatEvents[E <: OrderFatEvent: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

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
}
