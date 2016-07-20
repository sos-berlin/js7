package com.sos.scheduler.engine.common.async

import com.sos.scheduler.engine.base.utils.ScalaUtils.SwitchStatement
import com.sos.scheduler.engine.common.async.TimedCall._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.time.Instant
import java.util.concurrent.Callable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

trait TimedCall[A] extends Callable[A] {

  def epochMillis: Long

  def call(): A

  final def instant = Instant.ofEpochMilli(epochMillis)

  private[async] final def onApply(): Unit = {
    logger.trace(s"Calling $toString")
    val result = Try(call())
    callOnComplete(result)
    result switch {
      case Failure(t) if t.getClass.getName == "org.scalatest.exceptions.TestFailedException" ⇒ throw t  // Return this exception to C++ code, too
    }
  }

  private[async] final def onCancel(): Unit = {
    logger.debug(s"Cancel $toString")
    callOnComplete(Failure(new CancelledException(s"TimedCall cancelled: '$toString'")))
  }

  private def callOnComplete(o: Try[A]): Unit = {
    try onComplete(o)
    catch { case NonFatal(t) => logger.error(s"Error in onComplete() ignored: $t ($toString)", t) }
  }

  protected def onComplete(o: Try[A]): Unit = {
    o match {
      case Success(p) =>
      case Failure(e: CancelledException) =>
      case Failure(t) => logger.error(s"Error in TimedCall ignored: $t ($toString)", t)
    }
  }

  override def toString = {
    val s = Seq(
      Some(Try(toStringPrefix) getOrElse "toStringPrefix error"),
      Some(s"at=$atString") filter { _ => epochMillis != ShortTermMillis })
      .flatten mkString " "
    s"TimedCall '$s'"
  }

  final def atString =
    if (epochMillis == ShortTermMillis) "short-term"
    else instant.toString

  def toStringPrefix = getClass.getSimpleName
}

object TimedCall {
  /** So bald wie möglich. */
  private[async] val ShortTermMillis = 0
  /** So bald wie möglich. */
  private[async] val ShortTerm = Instant.ofEpochMilli(0)
  private val logger = Logger(getClass)

  def apply[A](at: Instant)(f: => A) = new TimedCall[A] {
    def epochMillis = at.toEpochMilli
    def call() = f
  }

  final class CancelledException protected[async](override val getMessage: String) extends RuntimeException
}
