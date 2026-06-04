package js7.controller.client

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import io.circe.{Decoder, Encoder, Json}
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.Locale
import js7.base.auth.Admission
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.Completed
import js7.base.log.LogLevel
import js7.base.log.reader.{KeyedByteLogLine, KeyedLogLine, LogLineKey, LogReaders, LogSelection}
import js7.base.problem.Checked.Ops
import js7.base.session.SessionApi
import js7.base.utils.Missing
import js7.base.utils.ScalaUtils.syntax.*
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

  protected final lazy val prefixedUri = baseUri / UriPrefixPath
  private lazy val metricsUri = baseUri / "metrics"

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
    logSelection: LogSelection = LogSelection.default,
    subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, fs2.Chunk[Byte]]] =
    getLogLines_(subagentId, logLevel,
      ("begin" -> begin.toString) +: logSelection.toKeyValues*)

  private def getLogLines_(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: (String, String | Missing)*)
  : IO[Stream[IO, fs2.Chunk[Byte]]] =
    loginAndRetryIfSessionLost:
      httpClient.getTextAsRawLines:
        toLogUri(subagentId, logLevel, queries)

  final def getKeyedLogLines(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    logSelection: LogSelection = LogSelection.default,
    subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, KeyedLogLine]] =
    getKeyedByteLogLines(logLevel, begin, logSelection, subagentId)
      .map(_.map(_.toKeyedLogLine))

  final def getKeyedByteLogLines(
    logLevel: LogLevel,
    begin: Instant | LogLineKey,
    logSelection: LogSelection = LogSelection.default,
    subagentId: Option[SubagentId] = None)
  : IO[Stream[IO, KeyedByteLogLine]] =
    getKeyedByteLogLines_(subagentId, logLevel,
      ("begin" -> begin.toString) +: logSelection.toKeyValues*)

  private def getKeyedLogLines_(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: (String, String | Missing)*)
  : IO[Stream[IO, KeyedLogLine]] =
    getKeyedByteLogLines_(subagentId, logLevel, queries*)
      .map(_.map(_.toKeyedLogLine))

  private def getKeyedByteLogLines_(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: (String, String | Missing)*)
  : IO[Stream[IO, KeyedByteLogLine]] =
    loginAndRetryIfSessionLost:
      httpClient.getTextAsRawLines:
        toLogUri(subagentId, logLevel, queries, withKey = true)
      .map:
        _.filter:
            _ != LogHeartbeat
          .map: byteLine =>
            KeyedByteLogLine.parse(byteLine).orThrow

  private def toLogUri(
    subagentId: Option[SubagentId],
    logLevel: LogLevel,
    queries: Seq[(String, String | Missing)],
    withKey: Boolean = false): Uri =
    val logLevelString = logLevel.toString.toLowerCase(Locale.ROOT)
    Uri:
      subagentId.match
        case None =>
          prefixedUri / "api" / "log" / logLevelString
        case Some(subagentId) =>
          prefixedUri / "api" / "subagent-forward" / subagentId.string / "log" / logLevelString
      .toString
        + encodeQuery(queries ++ (withKey ? ("withKey" -> "true")))

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

  def metrics(deep: Boolean): IO[fs2.Stream[IO, ByteString]] =
    loginAndRetryIfSessionLost:
      httpClient.getByteStream(
        Uri(metricsUri.string + (deep ?? "?deep=true")),
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
  private val LogHeartbeat = fs2.Chunk.array(LogReaders.LogHeartbeat.getBytes(UTF_8))

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
