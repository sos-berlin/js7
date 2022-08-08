package js7.data.cluster

import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.session.SessionApi
import js7.data.event.{Event, EventId, JournalPosition}
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.duration.FiniteDuration

trait ClusterNodeApi
extends SessionApi.HasUserAndPassword
with HasIsIgnorableStackTrace
{
  /** Observable for a journal file.
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

  def eventIdObservable[E <: Event](
    timeout: Option[FiniteDuration] = None,
    heartbeat: Option[FiniteDuration] = None)
  : Task[Observable[EventId]]

  def executeClusterCommand(command: ClusterCommand): Task[command.Response]
}
