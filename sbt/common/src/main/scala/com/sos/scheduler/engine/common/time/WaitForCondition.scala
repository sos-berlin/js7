package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.lang.System.currentTimeMillis
import java.time.Instant.now
import java.time.Duration

object WaitForCondition {

  /** Wartet längstens t.timeout in Schritten von t.step, bis condition wahr wird.
    * condition wird bei t.timeout > 0 wenigsten zweimal aufgerufen: am Anfang und am Ende.
    * @return letztes Ergebnis von condition */
  def waitForCondition(timeout: Duration, step: Duration)(condition: ⇒ Boolean): Boolean =
    waitForCondition(TimeoutWithSteps(timeout, step))(condition)

  /** Wartet längstens t.timeout in Schritten von t.step, bis condition wahr wird.
    * condition wird bei t.timeout > 0 wenigsten zweimal aufgerufen: am Anfang und am Ende.
    * @return letztes Ergebnis von condition */
  def waitForCondition(t: TimeoutWithSteps)(condition: ⇒ Boolean) =
    waitAtInstantsFor(t.toMillisInstantIterator(now()))(condition)

  /** Wartet bis zu den relative Zeitpunkten, bis condition wahr wird.
    * Die relative Zeitpunkt gelten ab jetzt (Instant.now).
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitFromNowFor(relativeInstants: TraversableOnce[Long])(condition: ⇒ Boolean) = {
    val nw = now()
    waitAtInstantsFor(relativeInstants map nw.toEpochMilli.+)(condition)
  }

  /** Wartet bis zu den Zeitpunkten, bis condition wahr wird.
    * condition wird am Anfang und am Ende geprüft.
    * @return letztes Ergebnis von condition */
  def waitAtInstantsFor(instants: TraversableOnce[Long])(condition: ⇒ Boolean) =
    realTimeIterator(instants) exists {_ ⇒ condition}

  /** Ein Iterator, der bei next() (oder hasNext) auf den nächsten Zeitpunkt wartet.
    * Wenn aufeinanderfolgende Zeitpunkte schon erreicht sind, kehrt der Iterator trotzdem jedesmal zurück.
    * Dass kann zu überflüssigen Aktionen des Aufrufers führen (aufeinanderfolgende Prüfung einer aufwändigen Bedingung). */
  def realTimeIterator(instants: TraversableOnce[Long]): Iterator[Unit] =
    instants.toIterator map sleepUntil // toIterator führt dazu, das now() erst bei next() oder hasNext lazy aufgerufen wird.

  private[time] def sleepUntil(until: Long): Unit = {
    val w = until - currentTimeMillis()
    if (w > 0) sleep(w)
  }
}
