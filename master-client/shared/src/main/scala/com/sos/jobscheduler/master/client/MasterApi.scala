package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderFatEvent, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, ObjectEncoder}
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait MasterApi {

  def executeCommand(command: MasterCommand): Task[command.MyResponse]

  def overview: Task[MasterOverview]

  def events[E <: Event: ClassTag](after: EventId, timeout: Duration)(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def fatEvents[E <: OrderFatEvent: ClassTag](after: EventId, timeout: Duration)(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
    : Task[TearableEventSeq[Seq, KeyedEvent[E]]]

  def ordersOverview: Task[OrdersOverview]

  def orders: Task[Stamped[Seq[Order[Order.State]]]]

  def workflows: Task[Stamped[Seq[Workflow]]]
}
