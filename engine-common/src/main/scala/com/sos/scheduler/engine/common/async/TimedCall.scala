package com.sos.scheduler.engine.common.async

import TimedCall._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.util.concurrent.Callable
import org.joda.time.Instant
import org.joda.time.format.ISODateTimeFormat
import scala.util.control.NonFatal
import scala.util.{Success, Try, Failure}

trait TimedCall[A] extends Callable[A] {

  def epochMillis: Long

  def call(): A

  final def instant = new Instant(epochMillis)

  private[async] final def onApply(): Unit = {
    logger.trace(s"Calling $toString")
    val result = Try(call()) match {
      case Failure(t) if t.getClass.getName == "org.scalatest.exceptions.TestFailedException" ⇒ throw t
      case o ⇒ o
    }
    callOnComplete(result)
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
    else ISODateTimeFormat.dateTime.print(epochMillis)

  def toStringPrefix = getClass.getSimpleName
}

object TimedCall {
  /** So bald wie möglich. */
  private[async] val ShortTermMillis = 0
  /** So bald wie möglich. */
  private[async] val ShortTerm = new Instant(0)
  private val logger = Logger(getClass)

  def apply[A](at: Instant)(f: => A) = new TimedCall[A] {
    def epochMillis = at.getMillis
    def call() = f
  }

  final class CancelledException protected[async](override val getMessage: String) extends RuntimeException
}
