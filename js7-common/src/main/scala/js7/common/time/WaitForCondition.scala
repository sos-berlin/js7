package js7.common.time

import js7.base.time.ScalaTime._
import scala.concurrent.blocking
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Success, Try}

object WaitForCondition
{
  def retryUntil[A](timeout: FiniteDuration, step: FiniteDuration)(body: => A): A = {
    val deadline = now + timeout
    try body
    catch { case NonFatal(t) =>
      while (deadline.hasTimeLeft()) {
        sleep(step min deadline.timeLeftOrZero)
        Try { body } match {
          case Success(result) => return result
          case _ =>
        }
      }
      throw t
    }
  }

  /** Wartet längstens t.timeout in Schritten von t.step, bis condition wahr wird.
    * condition wird bei t.timeout > 0 wenigsten zweimal aufgerufen: am Anfang und am Ende.
    * @return letztes Ergebnis von condition */
  def waitForCondition(timeout: FiniteDuration, step: FiniteDuration)(condition: => Boolean): Boolean =
    waitForCondition(TimeoutWithSteps(timeout, step))(condition)

  /** Wartet längstens t.timeout in Schritten von t.step, bis condition wahr wird.
    * condition wird bei t.timeout > 0 wenigsten zweimal aufgerufen: am Anfang und am Ende.
    * @return letztes Ergebnis von condition */
  def waitForCondition(t: TimeoutWithSteps)(condition: => Boolean): Boolean =
    waitAtDeadlinesFor(t.toDeadlineIterator(now))(condition)

  /** Wartet bis zu den relative Zeitpunkten, bis condition wahr wird.
    * Die relative Zeitpunkt gelten ab jetzt (Instant.now).
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitFromNowFor(deadlines: IterableOnce[Deadline])(condition: => Boolean): Boolean =
    waitAtDeadlinesFor(deadlines)(condition)

  /** Wartet bis zu den Zeitpunkten, bis condition wahr wird.
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitAtDeadlinesFor(instants: IterableOnce[Deadline])(condition: => Boolean) =
    blocking {
      realTimeIterator(instants) exists {_ => condition}
    }

  /** Ein Iterator, der bei next() (oder hasNext) auf den nächsten Zeitpunkt wartet.
    * Wenn aufeinanderfolgende Zeitpunkte schon erreicht sind, kehrt der Iterator trotzdem jedesmal zurück.
    * Dass kann zu überflüssigen Aktionen des Aufrufers führen (aufeinanderfolgende Prüfung einer aufwändigen Bedingung). */
  private[time] def realTimeIterator(deadlines: IterableOnce[Deadline]): Iterator[Unit] =
    deadlines.iterator map sleepUntil // .iterator führt dazu, dass now erst bei next() oder hasNext lazy aufgerufen wird.
}
