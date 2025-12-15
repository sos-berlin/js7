package js7.base.problem

import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
case class ProblemException private(problem: Problem, message: String, cause: Throwable | Null)
extends RuntimeException(message, cause.asInstanceOf[Throwable]):

  protected[problem] def this(problem: Problem, cause: Throwable) =
    this(problem, problem.message, cause)

  protected[problem] def this(problem: Problem) =
    this(problem, problem.toString, null)

  override def toString = s"ProblemException: $getMessage"


object ProblemException:
  @tailrec
  def unapply(throwable: Throwable): Option[Problem] =
    throwable match
      case e: ProblemException => Some(e.problem)
      case e: WrappedException => unapply(e.getCause)
      case _ => None

  private[problem] class NoStackTrace(problem: Problem, message: String, cause: Throwable | Null)
  extends
    ProblemException(problem, message, cause.asInstanceOf[Throwable]),
    scala.util.control.NoStackTrace:

    def this(problem: Problem, cause: Throwable | Null) =
      this(problem, problem.message, cause)

    def this(problem: Problem) =
      this(problem, problem.toString, null)

    override def toString = s"ProblemException: $getMessage"
