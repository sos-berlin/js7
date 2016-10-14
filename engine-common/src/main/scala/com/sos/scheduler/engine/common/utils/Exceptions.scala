package com.sos.scheduler.engine.common.utils

import java.time.{Duration, Instant}
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object Exceptions {

  def repeatUntilNoException[A](timeout: Duration, delayNext: Duration)(body: ⇒ A): A =
    repeatUntilNoException(until = Instant.now() plus timeout, delayNext)(body)

  def repeatUntilNoException[A](until: Instant, delayNext: Duration)(body: ⇒ A): A = {
    var tried = Try { body }
    while (tried.isFailure && (Instant.now isBefore until)) {
      Thread.sleep(delayNext.toMillis)
      tried = Try { body }
    }
    tried.get
  }

  private type LogFunction = (⇒ String, Throwable) ⇒ Unit

  def ignoreException[A](log: LogFunction)(body: ⇒ A): Try[A] =
    Try { body } recoverWith {
      case NonFatal(t)  ⇒
        log(s"Ignoring exception $t", t)
        Failure(t)
      }

  def logException[A](log: LogFunction)(body: ⇒ A): A =
    try body
    catch {
      case t: Throwable ⇒
        log(t.toString, t)
        throw t
    }
}
