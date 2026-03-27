package js7.base.utils

import js7.base.log.Logger
import js7.base.utils.Tests.isTest
import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object StackTraces:
  // FIXME Bad for constant exceptions which will get its stack trace growing

  private lazy val logger = Logger[this.type]

  private val eyecatcher =
    val bar = "________________________________________"
    new StackTraceElement(bar, bar, "appended", -1)

  private val NoStackTraceFor: Set[String] = Set(
    "org.apache.pekko.http.scaladsl.model.EntityStreamException",
    "org.apache.pekko.http.scaladsl.model.EntityStreamSizeException",
    "org.apache.pekko.http.scaladsl.model.IllegalHeaderException",
    "org.apache.pekko.http.scaladsl.model.IllegalRequestException",
    "org.apache.pekko.http.scaladsl.model.IllegalResponseException",
    "org.apache.pekko.http.scaladsl.model.InvalidContentLengthException",
    "org.apache.pekko.http.scaladsl.model.ParsingException",
    "org.apache.pekko.stream.StreamIdleTimeoutException")

  /**
    * Applicable for `Try`  of another context, like from a `Future`.
    * Modifies the original `Try` if it is a `Failure`.
    */
  implicit final class StackTraceTry[A](private val delegate: Try[A]) extends AnyVal:
    def appendCurrentStackTrace: Try[A] =
      delegate match
        case Failure(t) => t.appendStackTrace(new Exception().getStackTrace)
        case _ =>
      delegate

  implicit final class StackTraceThrowable[T <: Throwable](val delegate: T) extends AnyVal:
    // TODO Prefer WrappedException over modifying a Throwable!
    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendCurrentStackTrace: T =
      appendStackTrace(new Exception().getStackTrace
        .dropWhile(_.getClassName.startsWith("js7.base.utils.StackTraces")))

    // TODO Prefer WrappedException over modifying a Throwable!
    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendStackTrace(stackTrace: Array[StackTraceElement]): delegate.type =
      delegate.setStackTrace(
        if delegate.getStackTrace.isEmpty then
          stackTrace
        else
          (delegate.getStackTrace :+ eyecatcher) ++ stackTrace)

      if isTest then
        val limit = 5
        val st = delegate.getStackTrace
        if st.count(_ == eyecatcher) > limit then
          logger.warn:
            s"🔥🔥🔥 More than $limit appendStackTrace  for the same Exception_:\n  ${
              st.mkString("\n  ")}"

      delegate

  def hasRelevantStackTrace(throwable: Throwable): Boolean =
    !NoStackTraceFor.contains(throwable.getClass.getName)
