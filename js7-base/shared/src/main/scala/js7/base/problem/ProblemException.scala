package js7.base.problem

/**
  * @author Joacim Zschimmer
  */
case class ProblemException private(problem: Problem, message: String, cause: Throwable)
extends RuntimeException(message, cause):

  protected[problem] def this(problem: Problem, cause: Throwable) =
    this(problem, problem.message, cause)

  protected[problem] def this(problem: Problem) =
    this(problem, problem.toString, null)

  override def toString = s"ProblemException: $getMessage"


object ProblemException:
  def unapply(e: ProblemException) = Some(e.problem)

  private[problem] class NoStackTrace(problem: Problem, message: String, cause: Throwable)
  extends ProblemException(problem, message, cause), scala.util.control.NoStackTrace:
    def this(problem: Problem, cause: Throwable) =
      this(problem, problem.message, cause)

    def this(problem: Problem) =
      this(problem, problem.toString, null)

    override def toString = s"ProblemException: $getMessage"
