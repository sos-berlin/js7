package com.sos.jobscheduler.base.problem

/**
  * @author Joacim Zschimmer
  */
class ProblemException private(val problem: Problem, message: String, cause: Throwable)
extends RuntimeException(message, cause)
{
  protected[problem] def this(problem: Problem, cause: Throwable) =
    this(problem, problem.message, cause)

  protected[problem] def this(problem: Problem) =
    this(problem, problem.toString, null)

  override def toString = s"ProblemException: $getMessage"
}
