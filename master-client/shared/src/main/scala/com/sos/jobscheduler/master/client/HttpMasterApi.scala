package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.common.http.HttpClient
import com.sos.jobscheduler.data.event.{Event, EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrderFatEvent, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.client.HttpMasterApi._
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, ObjectEncoder}
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.reflect.ClassTag

trait HttpMasterApi extends MasterApi
{
  /** Host URI or empty for addressing base on "master/". */
  protected def uri: String
  protected def httpClient: HttpClient

  private lazy val uris = MasterUris(masterUri = if (uri.isEmpty) uri else uri.stripSuffix("/") + "/master")

  final def executeCommand(command: MasterCommand): Task[command.MyResponse] =
    httpClient.post[MasterCommand, MasterCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.MyResponse])

  final def overview: Task[MasterOverview] =
    httpClient.get[MasterOverview](uris.overview)

  final def addOrder(order: FreshOrder): Task[Boolean]  =
    httpClient.postIgnoreResponse(uris.order.add, order) map (_ == 201/*Created*/)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Task[Stamped[Seq[Order[Order.State]]]] =
    httpClient.get[Stamped[Seq[Order[Order.State]]]](uris.order.list[Order[Order.State]])

  final def events[E <: Event: ClassTag](after: EventId, timeout: Duration)(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.events[E](after = after, timeout = timeout),
      timeout = timeout + ToleratedEventDelay)

  final def fatEvents[E <: OrderFatEvent: ClassTag](after: EventId, timeout: Duration)(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.fatEvents[E](after = after, timeout = timeout),
      timeout = timeout + ToleratedEventDelay)

  final def workflows: Task[Stamped[Seq[Workflow]]] =
    httpClient.get[Stamped[Seq[Workflow]]](uris.workflow.list[Workflow])
}

object HttpMasterApi {
  private val ToleratedEventDelay = 30.seconds
}
