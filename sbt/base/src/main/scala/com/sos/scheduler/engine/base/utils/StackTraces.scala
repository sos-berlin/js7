package com.sos.scheduler.engine.base.utils

import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object StackTraces {

  /**
    * Applicable for `Try`  of another context, like from a `Future`.
    * Modifies the original `Try` if it is a `Failure`.
    */
  implicit class StackTraceTry[A](val delegate: Try[A]) extends AnyVal {
    def appendCurrentStackTrace = {
      delegate match {
        case Failure(t) ⇒ t.appendStackTrace(newStackTrace())
        case o ⇒ o
      }
      delegate
    }
  }

  implicit class StackTraceThrowable(val delegate: Throwable) extends AnyVal {
    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendCurrentStackTrace: delegate.type =
      appendStackTrace(newStackTrace())

    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendStackTrace(stackTrace: Array[StackTraceElement]): delegate.type = {
      delegate.setStackTrace(delegate.getStackTrace ++ stackTrace)
      delegate
    }
  }

  def newStackTrace() = new Exception().getStackTrace
}
