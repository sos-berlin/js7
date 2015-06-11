package com.sos.scheduler.engine.common.scalautil

import scala.util.{Failure, Try}

/**
 * @author Joacim Zschimmer
 */
object Tries {
  implicit class ModifiedStackTraceTry[A](val delegate: Try[A]) extends AnyVal {

    def withThisStackTrace: Try[A] = withStackTrace(newStackTrace())

    def withStackTrace(stackTrace: Array[StackTraceElement]): Try[A] = {
      delegate match {
        case Failure(t) ⇒ extendStackTraceWith(t, stackTrace)
        case o ⇒ o
      }
      delegate
    }
  }

  def extendStackTraceWith(t: Throwable, stackTrace: Array[StackTraceElement]): Unit = t.setStackTrace(t.getStackTrace ++ stackTrace)
  
  def newStackTrace() = new Exception().getStackTrace
}
