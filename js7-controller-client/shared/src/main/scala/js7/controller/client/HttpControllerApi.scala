package js7.controller.client

import cats.effect.Resource
import io.circe.{Decoder, Encoder, Json}
import js7.base.auth.UserAndPassword
import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.HttpClient.liftProblem
import js7.base.web.{HttpClient, Uri}
import js7.controller.client.HttpControllerApi._
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterNodeState, ClusterState}
import js7.data.controller.ControllerCommand.{DeleteOrdersWhenTerminated, InternalClusterCommand}
import js7.data.controller.{ControllerCommand, ControllerOverview, ControllerState}
import js7.data.event.{Event, EventApi, EventId, EventRequest, JournalInfo, JournalPosition, KeyedEvent, Stamped}
import js7.data.order.{FreshOrder, Order, OrderId, OrdersOverview}
import js7.data.session.HttpSessionApi
import monix.eval.Task
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

trait HttpControllerApi
extends EventApi with ClusterNodeApi with HttpSessionApi with HasIsIgnorableStackTrace
{
  type State = ControllerState

  def httpClient: HttpClient
  /** Host URI or empty for addressing base on "controller/" */
  def baseUri: Uri

  protected final def uriPrefixPath = UriPrefixPath

  protected final def sessionUri = uris.session

  final lazy val uris = ControllerUris(
    controllerUri =
      if (baseUri.isEmpty) baseUri
      else Uri(baseUri.string.stripSuffix("/") + "/controller"))

  final def post[A: Encoder, B: Decoder](uriTail: String, data: A): Task[B] =
    httpClient.post[A, B](baseUri /? uriTail, data)

  final def postObservable[A: Encoder: TypeTag, B: Decoder](uriTail: String, data: Observable[A]): Task[B] =
    httpClient.postObservable[A, B](baseUri /? uriTail, data)

  @TestOnly
  final def postObservableJsonString(uriTail: String, data: Observable[String]): Task[Json] =
    httpClient.postObservableJsonString(baseUri /? uriTail, data)

  final def get[B: Decoder](uriTail: String): Task[B] =
    httpClient.get[B](baseUri /? uriTail)

  final def executeClusterCommand(command: ClusterCommand): Task[command.Response] =
    executeCommand(InternalClusterCommand(command))
      .map(_.response.asInstanceOf[command.Response])

  final def executeCommand(command: ControllerCommand): Task[command.Response] =
    httpClient.post[ControllerCommand, ControllerCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.Response])

  final def overview: Task[ControllerOverview] =
    httpClient.get[ControllerOverview](uris.overview)

  final def clusterState: Task[Checked[ClusterState]] =
    liftProblem(
      httpClient.get[ClusterState](uris.clusterState))

  final def clusterNodeState: Task[ClusterNodeState] =
    httpClient.get[ClusterNodeState](uris.clusterNodeState)

  final def addOrder(order: FreshOrder): Task[Boolean] = {
    val uri = uris.order.add
    httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
      .map(_ == 201/*Created*/)
  }

  final def addOrders(orders: Seq[FreshOrder]): Task[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def deleteOrdersWhenTerminated(orderIds: Seq[OrderId]): Task[Completed] =
    executeCommand(DeleteOrdersWhenTerminated(orderIds))
      .map((_: ControllerCommand.Response.Accepted) => Completed)

  final def ordersOverview: Task[OrdersOverview] =
    httpClient.get[OrdersOverview](uris.order.overview)

  final def orders: Task[Checked[Seq[Order[Order.State]]]] =
    liftProblem(
      httpClient.get[Seq[Order[Order.State]]](uris.order.list[Order[Order.State]]))

  final def eventObservable[E <: Event: ClassTag](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](
      uris.events(request),
      responsive = true)

  final def eventIdObservable[E <: Event: ClassTag](
    timeout: Option[FiniteDuration] = None,
    heartbeat: Option[FiniteDuration] = None)
  : Task[Observable[EventId]] =
    httpClient.getDecodedLinesObservable[EventId](
      uris.eventIds(timeout, heartbeat = heartbeat),
      responsive = true)

  /** Observable for a journal file.
    * @param journalPosition start of observation
    * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
    */
  final def journalObservable(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None, timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false, returnAck: Boolean = false)
  : Task[Observable[ByteArray]] =
    httpClient.getRawLinesObservable(
      uris.journal(journalPosition, heartbeat = heartbeat,
        timeout = timeout, markEOF = markEOF, returnAck = returnAck))

  /** Observable for the growing flushed (and maybe synced) length of a journal file.
    * @param journalPosition start of observation
    * @param markEOF prepend every line with a space and return a last line "TIMEOUT\n" in case of timeout
    */
  final def journalLengthObservable(
    journalPosition: JournalPosition,
    timeout: FiniteDuration,
    markEOF: Boolean = false)
  : Task[Observable[Long]] =
    journalObservable(journalPosition,
      timeout = Some(timeout), markEOF = markEOF, returnAck = true
    ).map(_.map(_.utf8String.stripSuffix("\n").toLong))

  final def journalInfo: Task[JournalInfo] =
    httpClient.get[JournalInfo](uris.api("/journalInfo"))

  override def toString = s"HttpControllerApi($baseUri)"

  final def snapshot(eventId: Option[EventId] = None): Task[ControllerState] =
    snapshotAs[ControllerState](uris.snapshot.list(eventId))
}

object HttpControllerApi
{
  val UriPrefixPath = "/controller"
  private val logger = scribe.Logger[this.type]

  /** Logs out when the resource is being released. */
  def resource(
    uri: Uri,
    userAndPassword: Option[UserAndPassword],
    httpClient: HttpClient,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _)
  : Resource[Task, HttpControllerApi] =
    Resource.make(
      acquire = Task(new HttpControllerApi.Standard(uri, userAndPassword, httpClient, loginDelays))
    )(release = api =>
      api.logout()
        .void
        .onErrorHandle { t =>
          logger.debug(s"logout() => ${t.toStringWithCauses}")
          ()
        })

  final class Standard(
    val baseUri: Uri,
    protected val userAndPassword: Option[UserAndPassword],
    val httpClient: HttpClient,
    override protected val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _)
  extends HttpControllerApi
}
