package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.session.HttpSessionApi
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.base.web.HttpClient
import com.sos.jobscheduler.data.agent.AgentRef
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.order.{FreshOrder, Order, OrdersOverview}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.master.client.HttpMasterApi._
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview, MasterSnapshots}
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import monix.reactive.Observable
import scala.collection.immutable.Seq
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scodec.bits.ByteVector

trait HttpMasterApi extends MasterApi with HttpSessionApi
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

  final def addOrder(order: FreshOrder): Task[Boolean]  = {
    val uri = uris.order.add
    httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
      .map(_ == 201/*Created*/)
  }

  final def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Task[Stamped[Seq[Order[Order.State]]]] =
    httpClient.get[Stamped[Seq[Order[Order.State]]]](uris.order.list[Order[Order.State]])

  final def events[E <: Event: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.events[E](request),
      timeout = request.timeout.map(_ + ToleratedEventDelay) getOrElse Duration.Inf)

  final def eventObservable[E <: Event: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
    : Task[Observable[Stamped[KeyedEvent[E]]]] =
    httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](uris.events(request))

  final def eventIdObservable[E <: Event: ClassTag](request: EventRequest[E]): Task[Observable[EventId]] =
    httpClient.getDecodedLinesObservable[EventId](uris.events(request, eventIdOnly = true, onlyLastOfChunk = true))

  /** Observable for a journal file.
    * @param fileEventId denotes the journal file
    * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
    */
  final def journalObservable(fileEventId: EventId, position: Long, timeout: FiniteDuration, markEOF: Boolean = false,
    returnLength: Boolean = false)
  : Task[Observable[ByteVector]] =
    httpClient.getRawLinesObservable(
      uris.journal(fileEventId = fileEventId, position = position, timeout, markEOF = markEOF, returnLength = returnLength))

  /** Observable for the growing flushed (and maybe synced) length of a journal file.
    * @param fileEventId denotes the journal file
    * @param markEOF prepend every line with a space and return a last line "TIMEOUT\n" in case of timeout
    */
  final def journalLengthObservable(fileEventId: EventId, position: Long, timeout: FiniteDuration, markEOF: Boolean = false): Task[Observable[Long]] =
    journalObservable(fileEventId, position, timeout, markEOF, returnLength = true)
      .map(_.map(_.decodeUtf8.orThrow.stripSuffix("\n").toLong))

  final def fatEvents[E <: FatEvent: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.fatEvents[E](request),
      timeout = request.timeout.map(_ + ToleratedEventDelay) getOrElse Duration.Inf)

  final def workflows: Task[Stamped[Seq[Workflow]]] =
    httpClient.get[Stamped[Seq[Workflow]]](uris.workflow.list[Workflow])

  final def agents: Task[Stamped[Seq[AgentRef]]] =
    httpClient.get[Stamped[Seq[AgentRef]]](uris.agent.list[AgentRef])

  final def snapshot: Task[Stamped[Seq[Any]]] = {
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    httpClient.get[Stamped[Seq[Any]]](uris.snapshot.list)
  }

  override def toString = s"HttpMasterApi($baseUriString)"
}

object HttpMasterApi {
  private val ToleratedEventDelay = 30.seconds
}
