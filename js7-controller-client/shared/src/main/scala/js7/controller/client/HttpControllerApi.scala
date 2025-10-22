package js7.controller.client

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import io.circe.{Decoder, Encoder, Json}
import js7.base.auth.Admission
import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.session.SessionApi
import js7.base.web.{HttpClient, Uri}
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.controller.client.HttpControllerApi.*
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.controller.{ControllerCommand, ControllerOverview, ControllerState}
import js7.data.event.{EventApi, EventId, JournalInfo}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.session.HttpSessionApi
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

trait HttpControllerApi
extends EventApi, HttpClusterNodeApi, HttpSessionApi, HasIsIgnorableStackTrace:

  type State = ControllerState

  def httpClient: HttpClient
  /** Host URI or empty for addressing base on "controller/" */
  def baseUri: Uri

  protected final def prefixedUri = baseUri / UriPrefixPath

  //protected final def sessionUri = uris.session

  protected final lazy val uris = ControllerUris(
    controllerUri =
      if baseUri.isEmpty then baseUri
      else Uri(baseUri.string.stripSuffix("/") + "/controller"))

  final def post[A: Encoder, B: Decoder](uriTail: String, data: A): IO[B] =
    httpClient.post[A, B](baseUri /? uriTail, data)

  final def postStream[A: Encoder, B: Decoder](uriTail: String, stream: Stream[IO, A]): IO[B] =
    httpClient.postStream[A, B](baseUri /? uriTail, stream)

  @TestOnly
  final def postJsonStringStream(uriTail: String, stream: Stream[IO, String]): IO[Json] =
    httpClient.postJsonStringStream(baseUri /? uriTail, stream)

  final def get[B: Decoder](uriTail: String): IO[B] =
    httpClient.get[B](baseUri /? uriTail)

  final def getRawLinesStream(uriTail: String): IO[Stream[IO, ByteArray]] =
    httpClient.getRawLinesStream(baseUri /? uriTail)

  final def executeCommand[C <: ControllerCommand](command: C): IO[command.Response] =
    httpClient.post[ControllerCommand, ControllerCommand.Response](uris.command, command)
      .map(_.asInstanceOf[command.Response])

  //final def executeAgentCommand(agentPath: AgentPath, command: AgentCommand)
  //: IO[command.Response] =
  //  httpClient.post[AgentCommand, AgentCommand.Response](uris.agentCommand(agentPath), command)
  //    .map(_.asInstanceOf[command.Response])

  final def overview: IO[ControllerOverview] =
    httpClient.get[ControllerOverview](uris.overview)

  final def addOrder(order: FreshOrder): IO[Boolean] =
    val uri = uris.order.add
    httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
      .map(_ == 201/*Created*/)

  final def addOrders(orders: Seq[FreshOrder]): IO[Completed] =
    httpClient.postDiscardResponse(uris.order.add, orders)
      .map((_: Int) => Completed)

  final def deleteOrdersWhenTerminated(orderIds: Seq[OrderId]): IO[Completed] =
    executeCommand(DeleteOrdersWhenTerminated(orderIds))
      .map((_: ControllerCommand.Response.Accepted) => Completed)

  final def journalInfo: IO[JournalInfo] =
    httpClient.get[JournalInfo](uris.api("/journalInfo"))

  override def toString = s"HttpControllerApi($baseUri)"

  final def snapshot(eventId: Option[EventId] = None): IO[ControllerState] =
    snapshotAs[ControllerState](uris.snapshot.list(eventId))


object HttpControllerApi:
  val UriPrefixPath = "/controller"

  /** Logs out when the resource is being released. */
  def resource(
    admission: Admission,
    httpClient: HttpClient,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  : ResourceIO[HttpControllerApi] =
    SessionApi.resource(IO(
      new HttpControllerApi.Standard(admission, httpClient, loginDelays)))

  final class Standard(
    admission: Admission,
    val httpClient: HttpClient,
    override protected val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  extends HttpControllerApi:
    val baseUri: Uri = admission.uri
    protected val userAndPassword = admission.userAndPassword
