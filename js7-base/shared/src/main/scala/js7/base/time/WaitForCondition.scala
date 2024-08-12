package js7.base.time

import js7.base.time.ScalaTime.*
import scala.annotation.tailrec
import scala.concurrent.blocking
import scala.concurrent.duration.*
import scala.util.control.NonFatal

object WaitForCondition:

  private val defaultSleeper = RealtimeBlockingSleeper()

  /** Retry until no exception has been thrown or time has elapsed. */
  def retryUntil[A](timeout: FiniteDuration, step: FiniteDuration)(body: => A)
    (using sleeper: BlockingSleeper = defaultSleeper)
  : A =
    val deadline = sleeper.now + timeout
    try body
    catch case NonFatal(t) =>
      @tailrec def retryUntilSuccess(throwable: Throwable): A =
        val left = deadline.timeLeft min step
        if left.isZeroOrBelow then throw throwable
        sleeper.sleep(left)
        try body
        catch case NonFatal(t) => retryUntilSuccess(t)
      retryUntilSuccess(t)

  /** Wartet längstens t.timeout in Schritten von t.step, bis condition wahr wird.
    * condition wird bei t.timeout > 0 wenigsten zweimal aufgerufen: am Anfang und am Ende.
    * @return letztes Ergebnis von condition */
  def waitForCondition(timeout: FiniteDuration, step: FiniteDuration)(condition: => Boolean)
    (using sleeper: BlockingSleeper = defaultSleeper)
  : Boolean =
    waitAtDeadlinesFor(TimeoutWithSteps(timeout, step).toDeadlineIterator(0.s))(condition)

  /** Wartet bis zu den relative Zeitpunkten, bis condition wahr wird.
    * Die relative Zeitpunkt gelten ab jetzt (Instant.now).
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitFromNowFor(using sleeper: BlockingSleeper = defaultSleeper)(deadlines: IterableOnce[sleeper.Deadline])(condition: => Boolean)
  : Boolean =
    waitAtDeadlinesFor(deadlines)(condition)

  /** Wartet bis zu den Zeitpunkten, bis condition wahr wird.
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitAtDeadlinesFor(using sleeper: BlockingSleeper = defaultSleeper)(instants: IterableOnce[sleeper.Deadline])(condition: => Boolean)
  : Boolean =
    blocking:
      realTimeIterator(instants) exists {_ => condition}

  /** Ein Iterator, der bei next() (oder hasNext) auf den nächsten Zeitpunkt wartet.
    * Wenn aufeinanderfolgende Zeitpunkte schon erreicht sind, kehrt der Iterator trotzdem jedesmal zurück.
    * Dass kann zu überflüssigen Aktionen des Aufrufers führen (aufeinanderfolgende Prüfung einer aufwändigen Bedingung). */
  private[time] def realTimeIterator(using sleeper: BlockingSleeper = defaultSleeper)(deadlines: IterableOnce[sleeper.Deadline])
  : Iterator[Unit] =
    deadlines.iterator /*.iterator führt dazu, dass now erst bei next() oder hasNext lazy aufgerufen wird*/
      .map(sleeper.sleepUntil)
