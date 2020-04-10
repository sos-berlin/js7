package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.agent.AgentRef
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent
import com.sos.jobscheduler.data.order.{Order, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait MasterApi {

  def executeCommand(command: MasterCommand): Task[command.Response]

  def overview: Task[MasterOverview]

  def clusterState: Task[ClusterState]

  def events[E <: Event: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def fatEvents[E <: OrderFatEvent: ClassTag](eventRequest: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def ordersOverview: Task[OrdersOverview]

  def orders: Task[Stamped[Seq[Order[Order.State]]]]

  def workflows: Task[Stamped[Seq[Workflow]]]

  def agents: Task[Stamped[Seq[AgentRef]]]

  def snapshot: Task[Stamped[Seq[Any]]]
}
