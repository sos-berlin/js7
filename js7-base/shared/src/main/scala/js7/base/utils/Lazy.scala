package js7.base.utils

import cats.InvariantMonoidal
import java.util.concurrent.locks.ReentrantLock
import js7.base.utils.ScalaUtils.use

/**
  * Like Scala lazy but synchronization moved to own object.
  * `Lazy` is queryable about its state and detects recursive evaluation.
  *
  * @author Joacim Zschimmer
  */
sealed class Lazy[A] private(eval: => A, block: => Option[A] => Option[A]):

  private val lock = new ReentrantLock()
  @volatile
  private var state: State = NotEvaluated

  def apply(): A =
    value

  def value: A =
    state match
      case Evaluated(a) => a
      case _ =>
        recursionCheckedValue
          .getOrElse(throw new RecursiveLazyValueException(this))

  def recursionCheckedValue: Option[A] =
    state match
      case Evaluated(a) => Some(a)
      case _ =>
        block:
          lock.use:
            state match
              case Evaluated(a) => Some(a)
              case Evaluating => None
              case NotEvaluated =>
                state = Evaluating
                val a = eval
                state = Evaluated(a)
                Some(a)

  def whenDefined[F[_]](f: A => F[Unit])(using F: InvariantMonoidal[F]): F[Unit] =
    toOption.fold(F.unit)(f)

  def fold[B](ifEmpty: => B)(f: A => B): B =
    toOption.fold(ifEmpty)(f)

  def toOption: Option[A] =
    state match
      case Evaluated(a) => Some(a)
      case _ => None

  def isDefined: Boolean =
    state.isInstanceOf[Evaluated]

  def isEmpty: Boolean =
    !isDefined

  def foreach(f: A => Unit): Unit =
    state match
      case Evaluated(a) => f(a)
      case _ =>

  override def toString = s"Lazy($state)"

  private sealed trait State
  private case object NotEvaluated extends State
  private case object Evaluating extends State
  private sealed case class Evaluated(a: A @unchecked) extends State

  final class RecursiveLazyValueException(val origin: this.type)
  extends Exception("Recursive evaluation of Lazy")


object Lazy:

  def apply[A](eval: => A): Lazy[A] =
    blocking(eval)

  def blocking[A](eval: => A): Lazy[A] =
    new Lazy(eval, scala.concurrent.blocking(_))

  /** For very fast evaluations.
   * During evaluation other threads accessing the value are blocked. */
  def nonBlocking[A](eval: => A): Lazy[A] =
    new Lazy(eval, o => o)
