package js7.cluster.watch.api

import cats.effect.{IO, ResourceIO}
import fs2.Stream
import io.circe.Decoder
import js7.base.auth.Admission
import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.base.web.HttpClient.liftProblem
import js7.base.web.{HttpClient, Uri}
import js7.data.cluster.{ClusterCommand, ClusterNodeApi, ClusterNodeState, ClusterState, ClusterWatchId, ClusterWatchRequest, ClusterWatchingCommand}
import js7.data.event.{Event, EventId, EventRequest, JournalPosition, KeyedEvent, Stamped}
import js7.data.session.HttpSessionApi
import scala.concurrent.duration.*

trait HttpClusterNodeApi
extends ClusterNodeApi, HttpSessionApi, HasIsIgnorableStackTrace:

  def httpClient: HttpClient

  def baseUri: Uri

  // "http://.../controller" or ".../agent"
  protected def prefixedUri: Uri

  protected final def sessionUri = uris.session

  private lazy val uris = ClusterNodeUris(prefixedUri)

  final def clusterState: IO[Checked[ClusterState]] =
    liftProblem(
      httpClient.get[ClusterState](uris.clusterState))

  final def clusterNodeState: IO[ClusterNodeState] =
    httpClient.get[ClusterNodeState](uris.clusterNodeState)

  final def eventStream[E <: Event](
    request: EventRequest[E],
    heartbeat: Option[FiniteDuration] = None,
    idleTimeout: Option[FiniteDuration] = None)
    (implicit kd: Decoder[KeyedEvent[E]])
  : IO[Stream[IO, Stamped[KeyedEvent[E]]]] =
    retryIfSessionLost:
      httpClient.getDecodedLinesStream[Stamped[KeyedEvent[E]]](
        uris.events(request, heartbeat = heartbeat),
        responsive = true,
        idleTimeout = idleTimeout)

  final def eventIdStream[E <: Event](
    timeout: Option[FiniteDuration] = None,
    heartbeat: Option[FiniteDuration] = None,
    returnHeartbeatAs: Option[EventId] = None,
    dontLog: Boolean = false)
  : IO[Stream[IO, Checked[EventId]]] =
    import EventId.given_Codec_Checked
    retryIfSessionLost:
      httpClient
        .getDecodedLinesStream[Checked[EventId]](
          uris.eventIds(timeout, heartbeat = heartbeat),
          responsive = true,
          returnHeartbeatAs = for h <- returnHeartbeatAs yield ByteArray(h.toString),
          prefetch = 1000,
          dontLog = dontLog)
      .map(_
        .mapChunks: chunk =>
          // Only the lastest EventId in chunk
          fs2.Chunk.fromOption(chunk.last))
  /** Stream for a journal file.
    * @param journalPosition start of observation
    * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
    */
  final def journalStream(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None,
    returnHeartbeatAs: Option[ByteArray] = None,
    timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false,
    returnAck: Boolean = false)
  : IO[Stream[IO, ByteArray]] =
    httpClient.getRawLinesStream(
      uris.journal(journalPosition, heartbeat = heartbeat,
        timeout = timeout, markEOF = markEOF, returnAck = returnAck),
      returnHeartbeatAs = returnHeartbeatAs)

  /** Stream for the growing flushed (and maybe synced) length of a journal file.
    * @param journalPosition start of observation
    * @param markEOF prepend every line with a space and return a last line "TIMEOUT\n" in case of timeout
    */
  private final def journalLengthStream(
    journalPosition: JournalPosition,
    timeout: FiniteDuration,
    markEOF: Boolean = false)
  : IO[Stream[IO, Long]] =
    journalStream(journalPosition,
      timeout = Some(timeout), markEOF = markEOF, returnAck = true
    ).map(_.map(_.utf8String.stripSuffix("\n").toLong))

  final def clusterWatchRequestStream(
    clusterWatchId: ClusterWatchId,
    keepAlive: Option[FiniteDuration],
    dontLog: Boolean = false)
  : IO[Stream[IO, ClusterWatchRequest]] =
    retryIfSessionLost:
      httpClient.getDecodedLinesStream[ClusterWatchRequest](
        uris.clusterWatchMessages(clusterWatchId, keepAlive),
        dontLog = dontLog,
        responsive = true)

  final def executeClusterCommand(cmd: ClusterCommand): IO[cmd.Response] =
    httpClient.post[ClusterCommand, ClusterCommand.Response](uris.command, cmd)
      .map(_.asInstanceOf[cmd.Response])

  final def executeClusterWatchingCommand(cmd: ClusterWatchingCommand): IO[Unit] =
    httpClient.post[ClusterWatchingCommand, Unit](uris.command, cmd)

  override def toString = s"HttpClusterNodeApi($prefixedUri)"


object HttpClusterNodeApi:
  val UriPrefixPath = "/controller"

  /** Logs out when the resource is being released. */
  def resource(
    admission: Admission,
    httpClient: HttpClient,
    uriPrefix: String,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays)
  : ResourceIO[HttpClusterNodeApi] =
    SessionApi.resource(IO(
      new HttpClusterNodeApi.Standard(admission, httpClient, uriPrefix, loginDelays)))

  private final class Standard(
    admission: Admission,
    val httpClient: HttpClient,
    uriPrefix: String,
    override protected val loginDelays: () => Iterator[FiniteDuration] =
      SessionApi.defaultLoginDelays)
  extends HttpClusterNodeApi:
    val baseUri = admission.uri
    protected val prefixedUri = admission.uri / uriPrefix
    protected val userAndPassword = admission.userAndPassword
