package com.sos.jobscheduler.master.client
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, Encoder, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait MasterApi {

  def executeCommand(command: MasterCommand): Future[command.MyResponse]

  def overview: Future[MasterOverview]

  def events[E <: Event: ObjectEncoder: Decoder: ClassTag](after: EventId, timeout: Duration)(implicit kd: Decoder[E#Key], ke: Encoder[E#Key])
    : Future[Stamped[TearableEventSeq[Seq, KeyedEvent[E]]]]

  def ordersOverview: Future[OrdersOverview]

  def orders: Future[Stamped[Seq[Order[Order.State]]]]

  def workflows: Future[Stamped[Seq[Workflow]]]
}
