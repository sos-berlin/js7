package js7.base.time

import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.syntax.*
import monix.execution.Scheduler
import scala.concurrent.blocking
import scala.concurrent.duration.*
import scala.util.control.NonFatal
import scala.util.{Success, Try}

object WaitForCondition
{
  private[time] trait Sleeper {
    def now: MonixDeadline
    final def sleepUntil(until: MonixDeadline) = sleep(until - now)
    def sleep(duration: FiniteDuration): Unit
  }

  private val defaultSleeper = new StandardSleeper

  private class StandardSleeper extends Sleeper {
    def now = Scheduler.traced.now
    def sleep(duration: FiniteDuration) = ScalaTime.sleep(duration)
  }

  def retryUntil[A](timeout: FiniteDuration, step: FiniteDuration)(body: => A)
    (implicit sleeper: Sleeper = defaultSleeper)
  : A = {
    val deadline = sleeper.now + timeout
    try body
    catch { case NonFatal(t) =>
      while (deadline.hasTimeLeft) {
        sleeper.sleep(step min deadline.timeLeftOrZero)
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
  def waitForCondition(timeout: FiniteDuration, step: FiniteDuration)(condition: => Boolean)
    (implicit sleeper: Sleeper = defaultSleeper)
  : Boolean =
    waitAtDeadlinesFor(TimeoutWithSteps(timeout, step).toDeadlineIterator(sleeper.now))(condition)

  /** Wartet bis zu den relative Zeitpunkten, bis condition wahr wird.
    * Die relative Zeitpunkt gelten ab jetzt (Instant.now).
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitFromNowFor(deadlines: IterableOnce[MonixDeadline])(condition: => Boolean)
    (implicit sleeper: Sleeper = defaultSleeper)
  : Boolean =
    waitAtDeadlinesFor(deadlines)(condition)

  /** Wartet bis zu den Zeitpunkten, bis condition wahr wird.
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitAtDeadlinesFor(instants: IterableOnce[MonixDeadline])(condition: => Boolean)
    (implicit sleeper: Sleeper = defaultSleeper)
  : Boolean =
    blocking {
      realTimeIterator(instants) exists {_ => condition}
    }

  /** Ein Iterator, der bei next() (oder hasNext) auf den nächsten Zeitpunkt wartet.
    * Wenn aufeinanderfolgende Zeitpunkte schon erreicht sind, kehrt der Iterator trotzdem jedesmal zurück.
    * Dass kann zu überflüssigen Aktionen des Aufrufers führen (aufeinanderfolgende Prüfung einer aufwändigen Bedingung). */
  private[time] def realTimeIterator(deadlines: IterableOnce[MonixDeadline])
    (implicit sleeper: Sleeper = defaultSleeper)
  : Iterator[Unit] =
    deadlines.iterator /*.iterator führt dazu, dass now erst bei next() oder hasNext lazy aufgerufen wird*/
      .map(sleeper.sleepUntil)
}
