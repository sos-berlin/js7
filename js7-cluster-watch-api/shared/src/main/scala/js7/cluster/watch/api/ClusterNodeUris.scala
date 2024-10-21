package js7.cluster.watch.api

import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.Uri
import js7.base.web.Uris.encodeQuery
import js7.data.cluster.ClusterWatchId
import js7.data.event.{Event, EventRequest, JournalPosition}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

final class ClusterNodeUris private(prefixedUri: Uri):
  val command: Uri = api("/cluster/command")
  val session: Uri = api("/session")

  def clusterState: Uri = api("/cluster")

  def clusterNodeState: Uri = api("/cluster?return=ClusterNodeState")

  def clusterWatchMessages(clusterWatchId: ClusterWatchId, keepAlive: Option[FiniteDuration]): Uri =
    Uri(
      api("/cluster/clusterWatchMessage").string +
        encodeQuery(
          ("clusterWatchId" -> clusterWatchId.string) ::
            keepAlive.map("keepAlive" -> _.toDecimalString).toList))

  def events[E <: Event](
    request: EventRequest[E],
    heartbeat: Option[FiniteDuration] = None)
  : Uri =
    events_[E]("/event", request, heartbeat = heartbeat)

  private def events_[E <: Event](
    path: String,
    request: EventRequest[E],
    heartbeat: Option[FiniteDuration])
  : Uri =
    Uri(
      api(path).string + encodeQuery(
        (heartbeat.map("heartbeat" -> _.toDecimalString)) ++
          request.toQueryParameters))

  def eventIds(timeout: Option[FiniteDuration], heartbeat: Option[FiniteDuration] = None): Uri =
    Uri(
      api("/event").string + encodeQuery(
        Seq("onlyAcks" -> "true") ++
          timeout.map(o => "timeout" -> o.toDecimalString) ++
          heartbeat.map("heartbeat" -> _.toDecimalString)))

  def journal(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false,
    returnAck: Boolean = false): Uri
  = Uri(
    api("/journal").string + encodeQuery(
      (returnAck.thenList("return" -> "ack")) :::
      (heartbeat.map("heartbeat" -> _.toDecimalString)).toList :::
      (timeout.map("timeout" -> _.toDecimalString)).toList :::
      (markEOF.thenList("markEOF" -> "true")) :::
      ("file" -> journalPosition.fileEventId.toString) ::
      ("position" -> journalPosition.position.toString) :: Nil))

  def api(path: String, query: (String, String)*): Uri =
    if path.nonEmpty && !path.startsWith("/") then
      throw new IllegalArgumentException("Controller URI path must start with a slash")
    Uri(
      Uri(s"${prefixedUri}api$path").string + encodeQuery(query*))

  override def toString: String = prefixedUri.string


object ClusterNodeUris:
  def apply(prefixedUri: Uri): ClusterNodeUris =
    new ClusterNodeUris(Uri(prefixedUri.string.stripSuffix("/") + "/"))

  private def encodeClass[A: ClassTag]: String =
    encodeClass(implicitClass[A])

  private def encodeClass(cls: Class[?]): String =
    require(cls != classOf[Nothing], "Missing return=CLASS")
    cls.simpleScalaName
