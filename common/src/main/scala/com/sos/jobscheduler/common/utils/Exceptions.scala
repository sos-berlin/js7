package com.sos.jobscheduler.common.utils

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object Exceptions {
  private val logger = Logger(getClass)

  def repeatUntilNoException[A](timeout: FiniteDuration, delayNext: FiniteDuration)(body: => A): A =
    repeatUntilNoException(until = now + timeout, delayNext)(body)

  def repeatUntilNoException[A](until: Deadline, delayNext: FiniteDuration)(body: => A): A = {
    var tried = Try { body }
    while (tried.isFailure && until.hasTimeLeft()) {
      sleep(delayNext min until.timeLeftOrZero)
      tried = Try { body }
    }
    tried.get
  }

  private type LogFunction = (=> String, Throwable) => Unit

  def ignoreException[A](log: LogFunction)(body: => A): Try[A] =
    Try { body } recoverWith {
      case NonFatal(t)  =>
        log(s"Ignoring exception $t", t)
        Failure(t)
      }

  def logException[A](log: LogFunction)(body: => A): A =
    try body
    catch {
      case t: Throwable =>
        log(t.toString, t)
        throw t
    }

  def andRethrow(body: => Unit): PartialFunction[Throwable, Nothing] = {
    case NonFatal(t) =>
      onExceptionAddSuppressed(t) {
        body
      }
      throw t
  }

  /**
    * Catches an exception and tries to add it to `t` with `addSuppressed`.
    * If not supported, the exception is logged.
    */
  def onExceptionAddSuppressed(t: Throwable)(body: => Unit): Unit =
    try body
    catch {
      case suppressed: Throwable if suppressed ne t =>
        t.addSuppressed(suppressed)
        val suppresseds = t.getSuppressed
        if (suppresseds.isEmpty || (suppresseds.last ne suppressed)) // Suppression disabled?
          logger.warn(s"While handling an exception, this second exception is ignored: $suppressed\n" + s"Original exception is: $t", suppressed)
    }

  def wrapException[A](message: => String)(body: => A): A =
    try body
    catch { case NonFatal(t) =>
      throw new RuntimeException(s"$message: ${t.toSimplifiedString}", t)
    }
}
