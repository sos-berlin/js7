package js7.data.cluster

import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.data.event.{Event, EventId, JournalPosition}
import cats.effect.IO
import fs2.Stream
import scala.concurrent.duration.FiniteDuration

trait ClusterNodeApi
extends SessionApi.HasUserAndPassword, HasIsIgnorableStackTrace:

  def clusterState: IO[Checked[ClusterState]]

  def clusterNodeState: IO[ClusterNodeState]

  /** Stream for a journal file.
   *
   * @param journalPosition denotes journal file and position
   * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
   */
  def journalStream(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None,
    returnHeartbeatAs: Option[ByteArray] = None,
    timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false,
    returnAck: Boolean = false)
  : IO[Stream[IO, ByteArray]]

  //NOT USED
  //def journalLengthStream(
  //  journalPosition: JournalPosition,
  //  timeout: FiniteDuration,
  //  markEOF: Boolean = false)
  //: IO[Stream[IO, Long]]

  def eventIdStream[E <: Event](
    timeout: Option[FiniteDuration] = None,
    heartbeat: Option[FiniteDuration] = None,
    returnHeartbeatAs: Option[EventId] = None)
  : IO[Stream[IO, Checked[EventId]]]

  def clusterWatchRequestStream(
    clusterWatchId: ClusterWatchId,
    keepAlive: Option[FiniteDuration])
  : IO[Stream[IO, ClusterWatchRequest]]

  def executeClusterCommand(command: ClusterCommand): IO[command.Response]
