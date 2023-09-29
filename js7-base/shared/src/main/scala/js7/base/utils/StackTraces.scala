package js7.base.utils

import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object StackTraces:
  private val eyecatcher =
    val bar = "________________________________________"
    new StackTraceElement(bar, bar, "appended", -1)

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
    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendCurrentStackTrace: T =
      appendStackTrace(new Exception().getStackTrace
        .dropWhile(_.getClassName startsWith "js7.base.utils.StackTraces"))

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
      delegate
