package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.client.HttpMasterApi._
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
  * @param uri The host URI or empty for addressing base on "master/"
  */
trait HttpMasterApi extends MasterApi {
  protected def uri: String
  protected def httpClient: HttpClient
  protected implicit def executionContext: ExecutionContext

  private lazy val uris = MasterUris(masterUri = if (uri.isEmpty) uri else uri.stripSuffix("/") + "/master")

  final def executeCommand(command: MasterCommand): Future[command.MyResponse] =
    httpClient.post[MasterCommand, MasterCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.MyResponse])

  final def overview: Future[MasterOverview] =
    httpClient.get[MasterOverview](uris.overview)

  final def ordersOverview: Future[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Future[Stamped[Seq[Order[Order.State]]]] =
    httpClient.get[Stamped[Seq[Order[Order.State]]]](uris.order.list[Order[Order.State]])

  final def orderEvents(after: EventId, timeout: Duration): Future[Stamped[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]] =
    httpClient.get[Stamped[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]](
      uris.order.events[OrderEvent](after = after, timeout = timeout),
      timeout = timeout + ToleratedEventDelay)

  final def workflows: Future[Stamped[Seq[Workflow.Named]]] =
    httpClient.get[Stamped[Seq[Workflow.Named]]](uris.workflow.list[Workflow.Named])
}

object HttpMasterApi {
  private val ToleratedEventDelay = 30.seconds
}
