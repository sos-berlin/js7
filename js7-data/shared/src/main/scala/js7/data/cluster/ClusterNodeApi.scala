package js7.data.cluster

import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked
import js7.base.session.SessionApi
import js7.data.event.{Event, EventId, JournalPosition}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

trait ClusterNodeApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  def clusterState: Task[Checked[ClusterState]]

  def clusterNodeState: Task[ClusterNodeState]

  /** Observable for a journal file.
   *
   * @param journalPosition denotes journal file and position
   * @param markEOF mark EOF with the special line `JournalSeparators.EndOfJournalFileMarker`
   */
  def journalObservable(
    journalPosition: JournalPosition,
    heartbeat: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    markEOF: Boolean = false,
    returnAck: Boolean = false)
  : Task[Observable[ByteArray]]

  //NOT USED
  //def journalLengthObservable(
  //  journalPosition: JournalPosition,
  //  timeout: FiniteDuration,
  //  markEOF: Boolean = false)
  //: Task[Observable[Long]]

  def eventIdObservable[E <: Event](
    timeout: Option[FiniteDuration] = None,
    heartbeat: Option[FiniteDuration] = None)
  : Task[Observable[EventId]]

  def clusterWatchMessageObservable(heartbeat: Option[FiniteDuration])
  : Task[Observable[ClusterWatchMessage]]

  def executeClusterCommand(command: ClusterCommand): Task[command.Response]

  def executeClusterWatchCommand(cmd: ClusterWatchCommand): Task[Unit]
}
