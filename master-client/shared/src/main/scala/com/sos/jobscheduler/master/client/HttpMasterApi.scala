package js7.master.client

import cats.effect.Resource
import js7.base.auth.UserAndPassword
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.session.{HttpSessionApi, SessionApi}
import js7.base.utils.ScalaUtils._
import js7.base.web.{HttpClient, Uri}
import js7.data.agent.AgentRef
import js7.data.cluster.ClusterState
import js7.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.fatevent.FatEvent
import js7.data.order.{FreshOrder, Order, OrdersOverview}
import js7.data.workflow.Workflow
import js7.master.client.HttpMasterApi._
import js7.master.data.{MasterCommand, MasterOverview, MasterSnapshots}
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scodec.bits.ByteVector

trait HttpMasterApi
extends MasterApi with HttpSessionApi with HasIsIgnorableStackTrace
{
  def httpClient: HttpClient
  /** Host URI or empty for addressing base on "master/" */
  def baseUri: Uri

  protected final def uriPrefixPath = UriPrefixPath

  protected final def sessionUri = uris.session

  final lazy val uris = MasterUris(
    masterUri =
      if (baseUri.isEmpty) baseUri
      else Uri(baseUri.string.stripSuffix("/") + "/master"))

  final def post[A: Encoder, B: Decoder](uriTail: String, data: A): Task[B] =
    httpClient.post[A, B](Uri(baseUri.string.stripSuffix("/") + "/" + uriTail.stripPrefix("/")), data)

  final def executeCommand(command: MasterCommand): Task[command.Response] =
    httpClient.post[MasterCommand, MasterCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[MasterOverview] =
    httpClient.get[MasterOverview](uris.overview)

  final def clusterState: Task[ClusterState] =
    httpClient.get[ClusterState](uris.clusterState)

  final def addOrder(order: FreshOrder): Task[Boolean] = {
    val uri = uris.order.add
    httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
      .map(_ == 201/*Created*/)
  }

  final def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Task[Checked[Seq[Order[Order.State]]]] =
    httpClient.liftProblem(
      httpClient.get[Seq[Order[Order.State]]](uris.order.list[Order[Order.State]]))

  final def events[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.events[E](request),
      timeout = request.timeout.map(_ + ToleratedEventDelay) getOrElse Duration.Inf)

  final def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](uris.events(request))

  final def eventIdObservable[E <: Event: ClassTag](request: EventRequest[E], heartbeat: Option[FiniteDuration] = None)
  : Task[Observable[EventId]] =
    httpClient.getDecodedLinesObservable[EventId](uris.events(request, heartbeat = heartbeat, eventIdOnly = true, onlyLastOfChunk = true))

  /** Observable for a journal file.
    * @param fileEventId denotes the journal file
    * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
    */
  final def journalObservable(fileEventId: EventId, position: Long,
    heartbeat: Option[FiniteDuration] = None, timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false, returnLength: Boolean = false)
  : Task[Observable[ByteVector]] =
    httpClient.getRawLinesObservable(
      uris.journal(fileEventId = fileEventId, position = position,
        heartbeat = heartbeat, timeout = timeout, markEOF = markEOF, returnLength = returnLength))

  /** Observable for the growing flushed (and maybe synced) length of a journal file.
    * @param fileEventId denotes the journal file
    * @param markEOF prepend every line with a space and return a last line "TIMEOUT\n" in case of timeout
    */
  final def journalLengthObservable(fileEventId: EventId, position: Long, timeout: FiniteDuration, markEOF: Boolean = false): Task[Observable[Long]] =
    journalObservable(fileEventId, position, timeout = Some(timeout), markEOF = markEOF, returnLength = true)
      .map(_.map(_.decodeUtf8.orThrow.stripSuffix("\n").toLong))

  final def fatEvents[E <: FatEvent: ClassTag](request: EventRequest[E])(implicit kd: Decoder[KeyedEvent[E]], ke: Encoder.AsObject[KeyedEvent[E]])
  : Task[TearableEventSeq[Seq, KeyedEvent[E]]] =
    httpClient.get[TearableEventSeq[Seq, KeyedEvent[E]]](
      uris.fatEvents[E](request),
      timeout = request.timeout.map(_ + ToleratedEventDelay) getOrElse Duration.Inf)

  final def workflows: Task[Checked[Seq[Workflow]]] =
    httpClient.liftProblem(
      httpClient.get[Seq[Workflow]](uris.workflow.list[Workflow]))

  final def agents: Task[Checked[Seq[AgentRef]]] =
    httpClient.liftProblem(
      httpClient.get[Seq[AgentRef]](uris.agent.list[AgentRef]))

  final def snapshot: Task[Checked[Stamped[Seq[Any]]]] = {
    implicit val x = MasterSnapshots.SnapshotJsonCodec
    httpClient.liftProblem(
      httpClient.get[Stamped[Seq[Any]]](uris.snapshot.list))
  }

  override def toString = s"HttpMasterApi($baseUri)"
}

object HttpMasterApi
{
  val UriPrefixPath = "/master"
  private val ToleratedEventDelay = 30.seconds

  /** Logs out when the resource is being released. */
  def resource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    httpClient: HttpClient,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  : Resource[Task, HttpMasterApi] =
    Resource.make(
      acquire = Task(new HttpMasterApi.Standard(uri, userAndPassword, httpClient, loginDelays))
    )(release = api =>
      api.logout()
        .map(_ => ())
        .onErrorHandle(_ => ()))

  private class Standard(
    val baseUri: Uri,
    protected final val userAndPassword: Option[UserAndPassword],
    final val httpClient: HttpClient,
    override protected final val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  extends HttpMasterApi
}
