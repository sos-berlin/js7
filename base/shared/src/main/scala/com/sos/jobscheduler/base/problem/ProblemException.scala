package com.sos.jobscheduler.base.problem

/**
  * @author Joacim Zschimmer
  */
final class ProblemException private [problem](message: String, cause: Throwable = null)
extends RuntimeException(message, cause) {
  def this(cause: Throwable) = this(cause.toString, cause)
}
