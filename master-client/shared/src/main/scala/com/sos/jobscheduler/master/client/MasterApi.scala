package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.data.event.{EventId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.{Order, OrderEvent}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.client.MasterApi._
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
  * @param uri The host URI or empty for addressing base on "master/"
  */
class MasterApi(httpClient: HttpClient, uri: String)(implicit executionContext: ExecutionContext) {

  private val uris = MasterUris(masterUri = if (uri.isEmpty) uri else uri.stripSuffix("/") + "/master")

  def executeCommand(command: MasterCommand): Future[command.MyResponse] =
    httpClient.post[MasterCommand, MasterCommand.Response](uris.command, command)
      .map (_.asInstanceOf[command.MyResponse])

  def overview: Future[MasterOverview] =
    httpClient.get[MasterOverview](uris.overview)

  def orders: Future[Stamped[Seq[Order[Order.State]]]] =
    httpClient.get[Stamped[Seq[Order[Order.State]]]](uris.order.list[Order[Order.State]])

  def orderEvents(after: EventId, timeout: Duration): Future[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]](
      uris.order.events[OrderEvent](after = after, timeout = timeout),
      timeout = timeout + ToleratedEventDelay)

  def workflows: Future[Stamped[Seq[Workflow]]] =
    httpClient.get[Stamped[Seq[Workflow]]](uris.workflow.list[Workflow])
}

object MasterApi {
  private val ToleratedEventDelay = 30.seconds
}
