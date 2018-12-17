package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import java.time.{Duration, Instant}
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object Exceptions {
  private val logger = Logger(getClass)

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

  def andRethrow(body: ⇒ Unit): PartialFunction[Throwable, Nothing] = {
    case NonFatal(t) ⇒
      onExceptionAddSuppressed(t) {
        body
      }
      throw t
  }

  /**
    * Catches an exception and tries to add it to `t` with `addSuppressed`.
    * If not supported, the exception is logged.
    */
  def onExceptionAddSuppressed(t: Throwable)(body: ⇒ Unit): Unit =
    try body
    catch {
      case suppressed: Throwable if suppressed ne t ⇒
        t.addSuppressed(suppressed)
        val suppresseds = t.getSuppressed
        if (suppresseds.isEmpty || (suppresseds.last ne suppressed)) // Suppression disabled?
          logger.warn(s"While handling an exception, this second exception is ignored: $suppressed\n" + s"Original exception is: $t", suppressed)
    }

  def wrapException[A](message: ⇒ String)(body: ⇒ A): A =
    try body
    catch { case NonFatal(t) ⇒
      throw new RuntimeException(s"$message: ${t.toSimplifiedString}", t)
    }
}
