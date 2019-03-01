package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.session.SessionApi
import com.sos.jobscheduler.base.web.HttpClient
import com.sos.jobscheduler.data.agent.AgentRef
import com.sos.jobscheduler.data.event.{Event, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.client.HttpMasterApi._
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import io.circe.{Decoder, ObjectEncoder}
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.reflect.ClassTag

trait HttpMasterApi extends MasterApi with SessionApi
{
  /** Host URI or empty for addressing base on "master/". */
  protected def baseUriString: String

  protected def uriPrefixPath = "/master"

  protected def httpClient: HttpClient

  protected final def sessionUri = uris.session

  lazy val uris = MasterUris(masterUri = if (baseUriString.isEmpty) baseUriString else baseUriString.stripSuffix("/") + "/master")

  final def executeCommand(command: MasterCommand): Task[command.Response] =
    httpClient.post[MasterCommand, MasterCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[MasterOverview] =
    httpClient.get[MasterOverview](uris.overview)

  final def addOrder(order: FreshOrder): Task[Boolean]  =
    httpClient.postDiscardResponse(uris.order.add, order) map (_ == 201/*Created*/)

  final def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Task[Stamped[Seq[Order[Order.State]]]] =
    httpClient.get[Stamped[Seq[Order[Order.State]]]](uris.order.list[Order[Order.State]])

  final def events[E <: Event: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.events[E](request),
      timeout = request.timeout + ToleratedEventDelay)

  final def fatEvents[E <: FatEvent: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: ObjectEncoder[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.fatEvents[E](request),
      timeout = request.timeout + ToleratedEventDelay)

  final def workflows: Task[Stamped[Seq[Workflow]]] =
    httpClient.get[Stamped[Seq[Workflow]]](uris.workflow.list[Workflow])

  final def agents: Task[Stamped[Seq[AgentRef]]] =
    httpClient.get[Stamped[Seq[AgentRef]]](uris.agent.list[AgentRef])
}

object HttpMasterApi {
  private val ToleratedEventDelay = 30.seconds
}
