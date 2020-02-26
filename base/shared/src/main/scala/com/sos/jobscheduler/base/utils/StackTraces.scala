package com.sos.jobscheduler.base.utils

import scala.util.{Failure, Try}

/**
  * @author Joacim Zschimmer
  */
object StackTraces
{
  private val eyecatcher = {
    val bar = "________________________________________"
    new StackTraceElement(bar, bar, "appended", -1)
  }

  /**
    * Applicable for `Try`  of another context, like from a `Future`.
    * Modifies the original `Try` if it is a `Failure`.
    */
  implicit final class StackTraceTry[A](private val delegate: Try[A]) extends AnyVal {
    def appendCurrentStackTrace: Try[A] = {
      delegate match {
        case Failure(t) => t.appendStackTrace(new Exception().getStackTrace)
        case _ =>
      }
      delegate
    }
  }

  implicit final class StackTraceThrowable[T <: Throwable](val delegate: T) extends AnyVal {
    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendCurrentStackTrace: T =  // delegate.type: inferred existential type _1.delegate.type forSome { val _1: com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable }, which cannot be expressed by wildcards, should be enabled
      appendStackTrace(new Exception().getStackTrace)

    /**
      * Applicable for Throwables of another context, like from a `Future`.
      * Modifies the original `Throwable`.
      */
    def appendStackTrace(stackTrace: Array[StackTraceElement]): delegate.type = {
      delegate.setStackTrace(
        if (delegate.getStackTrace.isEmpty)
          stackTrace
        else
          (delegate.getStackTrace :+ eyecatcher) ++ stackTrace)
      delegate
    }
  }
}
