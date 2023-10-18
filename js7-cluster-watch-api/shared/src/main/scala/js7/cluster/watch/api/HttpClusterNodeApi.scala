package js7.cluster.watch.api

import cats.effect.Resource
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
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.*

trait HttpClusterNodeApi
extends ClusterNodeApi, HttpSessionApi, HasIsIgnorableStackTrace:
  
  def httpClient: HttpClient

  def baseUri: Uri

  // "http://.../controller" or ".../agent"
  protected def prefixedUri: Uri

  protected final def sessionUri = uris.session

  private lazy val uris = ClusterNodeUris(prefixedUri)

  final def clusterState: Task[Checked[ClusterState]] =
    liftProblem(
      httpClient.get[ClusterState](uris.clusterState))

  final def clusterNodeState: Task[ClusterNodeState] =
    httpClient.get[ClusterNodeState](uris.clusterNodeState)

  final def eventObservable[E <: Event](request: EventRequest[E])
    (implicit kd: Decoder[KeyedEvent[E]])
  : Task[Observable[Stamped[KeyedEvent[E]]]] =
    httpClient.getDecodedLinesObservable[Stamped[KeyedEvent[E]]](
      uris.events(request),
      responsive = true)

  final def eventIdObservable[E <: Event](
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
  private final def journalLengthObservable(
    journalPosition: JournalPosition,
    timeout: FiniteDuration,
    markEOF: Boolean = false)
  : Task[Observable[Long]] =
    journalObservable(journalPosition,
      timeout = Some(timeout), markEOF = markEOF, returnAck = true
    ).map(_.map(_.utf8String.stripSuffix("\n").toLong))

  final def clusterWatchRequestObservable(
    clusterWatchId: ClusterWatchId,
    keepAlive: Option[FiniteDuration])
  : Task[Observable[ClusterWatchRequest]] =
    httpClient.getDecodedLinesObservable[ClusterWatchRequest](
      uris.clusterWatchMessages(clusterWatchId, keepAlive),
      responsive = true)

  final def executeClusterCommand(cmd: ClusterCommand): Task[cmd.Response] =
    httpClient.post[ClusterCommand, ClusterCommand.Response](uris.command, cmd)
      .map(_.asInstanceOf[cmd.Response])

  final def executeClusterWatchingCommand(cmd: ClusterWatchingCommand): Task[Unit] =
    httpClient.post[ClusterWatchingCommand, Unit](uris.command, cmd)

  override def toString = s"HttpClusterNodeApi($prefixedUri)"


object HttpClusterNodeApi:
  val UriPrefixPath = "/controller"

  /** Logs out when the resource is being released. */
  def resource(
    admission: Admission,
    httpClient: HttpClient,
    uriPrefix: String,
    loginDelays: () => Iterator[FiniteDuration] = SessionApi.defaultLoginDelays _)
  : Resource[Task, HttpClusterNodeApi] =
    SessionApi.resource(Task(
      new HttpClusterNodeApi.Standard(admission, httpClient, uriPrefix, loginDelays)))

  private final class Standard(
    admission: Admission,
    val httpClient: HttpClient,
    uriPrefix: String,
    override protected val loginDelays: () => Iterator[FiniteDuration] =
      SessionApi.defaultLoginDelays _)
  extends HttpClusterNodeApi:
    val baseUri = admission.uri
    protected val prefixedUri = admission.uri / uriPrefix
    protected val userAndPassword = admission.userAndPassword
