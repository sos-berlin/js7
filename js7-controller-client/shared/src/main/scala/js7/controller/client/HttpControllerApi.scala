package js7.controller.client

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import io.circe.{Decoder, Encoder, Json}
import java.time.Instant
import java.util.Locale
import js7.base.auth.Admission
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.log.LogLevel
import js7.base.log.reader.{KeyedLogLine, LogLineKey}
import js7.base.session.SessionApi
import js7.base.utils.Missing
import js7.base.utils.Missing.toMissing
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.cluster.watch.api.HttpClusterNodeApi
import js7.common.http.PekkoHttpClient
import js7.common.metrics.MetricsProvider
import js7.controller.client.HttpControllerApi.*
import js7.data.controller.ControllerCommand.DeleteOrdersWhenTerminated
import js7.data.controller.{ControllerCommand, ControllerOverview, ControllerState}
import js7.data.event.{EventApi, EventId, JournalInfo}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.session.HttpSessionApi
import js7.data.subagent.SubagentId
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

trait HttpControllerApi
extends EventApi, HttpClusterNodeApi, HttpSessionApi, HasIsIgnorableStackTrace:

  type State = ControllerState

  def httpClient: PekkoHttpClient
  /** Host URI or empty for addressing base on "controller/" */
  def baseUri: Uri

  protected final def prefixedUri = baseUri / UriPrefixPath

  //protected final def sessionUri = uris.session

  protected final lazy val uris = ControllerUris(
    controllerUri =
      if baseUri.isEmpty then baseUri
      else Uri(baseUri.string.stripSuffix("/") + "/controller"))

  final def post[A: Encoder, B: Decoder](uriTail: String, data: A): IO[B] =
    loginAndRetryIfSessionLost:
      httpClient.post[A, B](baseUri /? uriTail, data)

  final def postStream[A: Encoder, B: Decoder](uriTail: String, stream: Stream[IO, A]): IO[B] =
    loginAndRetryIfSessionLost:
      httpClient.postStream[A, B](baseUri /? uriTail, stream)

  @TestOnly
  final def postJsonStringStream(uriTail: String, stream: Stream[IO, String]): IO[Json] =
    loginAndRetryIfSessionLost:
      httpClient.postJsonStringStream(baseUri /? uriTail, stream)

  final def get[B: Decoder](uriTail: String): IO[B] =
    loginAndRetryIfSessionLost:
      httpClient.get[B](baseUri /? uriTail)

  final def getNewLogLines(logLevel: LogLevel, subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, fs2.Chunk[Byte]]] =
    getLogLines_(subagentId, logLevel)

  final def getLogLines(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: Option[Long] = None,
    subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, fs2.Chunk[Byte]]] =
    getLogLines_(subagentId, logLevel,
      "begin" -> begin.toString,
      "lines" -> lines.map(_.toString).toMissing)

  private def getLogLines_(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: (String, String | Missing)*)
  : IO[Stream[IO, fs2.Chunk[Byte]]] =
    loginAndRetryIfSessionLost:
      val path = subagentId match
        case None => prefixedUri / "api" / "log" / logLevel.toString.toLowerCase(Locale.ROOT)
        case Some(subagentId) =>
          prefixedUri / "api" / "subagent-forward" / subagentId.string / "log" /
            logLevel.toString.toLowerCase(Locale.ROOT)
      httpClient.getTextAsRawLines(Uri(path.toString + encodeQuery(queries*)))

  final def getKeyedLogLines(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    lines: Option[Long],
    subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, KeyedLogLine]] =
    getKeyedLogLines_(subagentId, logLevel,
      "begin" -> begin.toString,
      "lines" -> lines.map(_.toString).toMissing)

  private def getKeyedLogLines_(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: (String, String | Missing)*)
  : IO[Stream[IO, KeyedLogLine]] =
    loginAndRetryIfSessionLost:
      httpClient.getDecodedLinesStream[KeyedLogLine](
        Uri:
          subagentId.match
            case None =>
              (prefixedUri / "api" / "log" / logLevel.toString.toLowerCase(Locale.ROOT)).toString
            case Some(subagentId) =>
              (prefixedUri / "api" / "subagent-forward" / subagentId.string / logLevel.toString.toLowerCase(Locale.ROOT)).toString
          + encodeQuery(queries*),
          dontLog = true)

  @TestOnly
  final def getRawLinesStream(uriTail: String): IO[Stream[IO, fs2.Chunk[Byte]]] =
    loginAndRetryIfSessionLost:
      httpClient.getTextAsRawLines(baseUri /? uriTail)

  final def executeCommand[C <: ControllerCommand](command: C): IO[command.Response] =
    loginAndRetryIfSessionLost:
      httpClient.post[ControllerCommand, ControllerCommand.Response](uris.command, command)
        .map(_.asInstanceOf[command.Response])

  //final def executeAgentCommand(agentPath: AgentPath, command: AgentCommand)
  //: IO[command.Response] =
  //  httpClient.post[AgentCommand, AgentCommand.Response](uris.agentCommand(agentPath), command)
  //    .map(_.asInstanceOf[command.Response])

  def metrics: IO[fs2.Stream[IO, ByteString]] =
    loginAndRetryIfSessionLost:
      httpClient.getByteStream(
        baseUri / "metrics",
        MetricsProvider.PrometheusRequestHeaders,
        dontLog = true)

  final def overview: IO[ControllerOverview] =
    loginAndRetryIfSessionLost:
      httpClient.get[ControllerOverview](uris.overview)

  final def addOrder(order: FreshOrder): IO[Boolean] =
    val uri = uris.order.add
    loginAndRetryIfSessionLost:
      httpClient.postDiscardResponse(uri, order, allowedStatusCodes = Set(409))
        .map(_ == 201/*Created*/)

  final def addOrders(orders: Seq[FreshOrder]): IO[Completed] =
    loginAndRetryIfSessionLost:
      httpClient.postDiscardResponse(uris.order.add, orders)
        .map((_: Int) => Completed)

  final def deleteOrdersWhenTerminated(orderIds: Seq[OrderId]): IO[Completed] =
    executeCommand(DeleteOrdersWhenTerminated(orderIds))
      .map((_: ControllerCommand.Response.Accepted) => Completed)

  final def journalInfo: IO[JournalInfo] =
    loginAndRetryIfSessionLost:
      httpClient.get[JournalInfo](uris.api("/journalInfo"))

  final def snapshot(eventId: Option[EventId] = None): IO[ControllerState] =
    snapshotAs[ControllerState](uris.snapshot.list(eventId))

  override def toString = s"HttpControllerApi($baseUri)"


object HttpControllerApi:
  val UriPrefixPath = "/controller"

  /** Logs out when the resource is being released. */
  def resource(
    admission: Admission,
    httpClient: PekkoHttpClient,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  : ResourceIO[HttpControllerApi] =
    SessionApi.logoutOnRelease:
      IO:
        new HttpControllerApi.Standard(admission, httpClient, loginDelays)

  final class Standard(
    admission: Admission,
    val httpClient: PekkoHttpClient,
    override protected val loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  extends HttpControllerApi:
    val baseUri: Uri = admission.uri
    protected val userAndPassword = admission.userAndPassword
