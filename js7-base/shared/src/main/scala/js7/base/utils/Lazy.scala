package js7.base.utils

/**
  * Like Scala lazy but synchronization moved to own object.
  * `Lazy` is queryable about its state and detects recursive evaluation.
  *
  * @author Joacim Zschimmer
  */
import js7.base.problem.Problem

final class Lazy[A] private(eval: => A)
{
  @volatile
  private var state: State = NotEvaluated

  def apply(): A = value

  def value: A =
    state match {
      case Evaluated(a) => a
      case _ =>
        recursionCheckedValue
          .getOrElse(throw new RecursiveLazyValueException(this))
    }

  def recursionCheckedValue: Option[A] =
    state match {
      case Evaluated(a) => Some(a)
      case _ =>
        synchronized {
          state match {
            case Evaluated(a) => Some(a)
            case Evaluating => None
            case NotEvaluated =>
              state = Evaluating
              val a = eval
              state = Evaluated(a)
              Some(a)
          }
        }
    }

  def toOption: Option[A] =
    state match {
      case Evaluated(a) => Some(a)
      case _ => None
    }

  def isDefined = state.isInstanceOf[Evaluated]

  def isEmpty = !isDefined

  def foreach(f: A => Unit): Unit =
    state match {
      case Evaluated(a) => f(a)
      case _ =>
    }

  override def toString = s"Lazy($state)"

  private sealed trait State
  private case object NotEvaluated extends State
  private case object Evaluating extends State
  private sealed case class Evaluated(a: A @unchecked) extends State

  final class RecursiveLazyValueException(val origin: this.type)
  extends Exception("Recursive evaluation of Lazy")
}

object Lazy
{
  def apply[A](eval: => A): Lazy[A] =
    new Lazy(eval)

  case object RecursiveLazyEvaluationProblem extends Problem.ArgumentlessCoded
}
