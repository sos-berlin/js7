package js7.base.utils

import cats.InvariantMonoidal
import cats.effect.IO
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.fromFutureDummyCancelable
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import scala.concurrent.{Future, Promise}
import scala.util.Success

/**
 * Variable which can be set only once. Thread-safe.
 *
 * @author Joacim Zschimmer
 */
final class SetOnce[A](label: => String, notYetSetProblem: Problem):

  protected[this] val promise = Promise[A]()

  /** May leak on cancel, until SetOnce is garbage-collected. */
  lazy val io: IO[A] =
    IO.fromFutureDummyCancelable(IO.pure(future)).unsafeMemoize

  override def toString = toStringOr(s"SetOnce[$label](not yet set)")

  @inline def toStringOr(or: => String): String =
    promise.future.value match
      case None => or
      case Some(Success(o)) => o.toString
      case Some(o) => o.toString  // Never happens

  def getOrUpdate(lazyValue: => A): A =
    promise.future.value match
      case Some(o) => o.get
      case None =>
        synchronized:
          promise.future.value match
            case Some(o) => o.get
            case None =>
              val value = lazyValue
              promise.success(value)
              value

  def whenDefined[F[_]](f: A => F[Unit])(using F: InvariantMonoidal[F]): F[Unit] =
    toOption.fold(F.unit)(f)

  def fold[B](ifEmpty: => B)(f: A => B): B =
    toOption.fold(ifEmpty)(f)

  def toOption: Option[A] =
    promise.future.value.map(_.get)

  def future: Future[A] =
    promise.future

  def isDefined = nonEmpty

  def isEmpty = !nonEmpty

  def nonEmpty = promise.future.isCompleted

  def contains(a: A) =
    toOption contains a

  def foreach(f: A => Unit) =
    toOption.foreach(f)

  /**
   * Sets the variable.
   * Thread-safe.
   * May be called only once.
   *
   * @throws IllegalStateException
   */
  def :=(value: A): A =
    if !trySet(value) then throw new IllegalStateException(s"SetOnce[$label] has already been set")
    value

  /** @return true iff the value has not yet been set. */
  def trySet(value: A): Boolean =
    promise.trySuccess(value)

  def orThrow: A =
    checked.orThrow

  def getOrElse[B >: A](els: => B): B =
    checked getOrElse els

  def checked: Checked[A] =
    promise.future.value match
      case None => Left(notYetSetProblem)
      case Some(o) => Right(o.get)


object SetOnce:
  def apply[A](implicit A: Tag[A]): SetOnce[A] =
    lazy val label = A.tag.toString
    SetOnce[A](label)

  def apply[A](problem: Problem)(implicit A: Tag[A]): SetOnce[A] =
    undefined[A](problem)

  def apply[A](label: => String): SetOnce[A] =
    new SetOnce[A](label, Problem(s"SetOnce[$label] promise has not been kept so far"))

  def apply[A](label: String, problem: Problem): SetOnce[A] =
    new SetOnce[A](label, problem)

  def undefined[A](problem: Problem)(implicit A: Tag[A]): SetOnce[A] =
    lazy val label = A.tag.toString  // Seems to be slow
    new SetOnce[A](label, problem)

  def undefined[A](label: => String, problem: Problem): SetOnce[A] =
    new SetOnce[A](label, problem)

  def fromOption[A](option: Option[A])(implicit A: Tag[A]) =
    val setOnce = SetOnce[A](A.tag.toString)
    option foreach setOnce.:=
    setOnce

  def fromOption[A](option: Option[A], label: String) =
    val setOnce = SetOnce[A](label)
    option foreach setOnce.:=
    setOnce
