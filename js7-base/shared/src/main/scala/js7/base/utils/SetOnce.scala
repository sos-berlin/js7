package js7.base.utils

import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import monix.eval.Task
import scala.concurrent.{Future, Promise}
import scala.reflect.runtime.universe._
import scala.util.Success

/**
 * Variable which can be set only once. Thread-safe.
 *
 * @author Joacim Zschimmer
 */
class SetOnce[A](label: String)
{
  protected[this] val promise = Promise[A]

  final override def toString = toStringOr(s"SetOnce[$label](not yet set)")

  @inline final def toStringOr(or: => String): String =
    promise.future.value match {
      case None => or
      case Some(Success(o)) => o.toString
      case Some(o) => o.toString  // Never happens
    }

  final def getOrUpdate(value: => A): A =
    promise.future.value match {
      case Some(o) => o.get
      case None =>
        promise.trySuccess(value)  // When concurrently called, the value is discarded
        promise.future.value.get.get
    }

  final def toOption: Option[A] =
    promise.future.value.map(_.get)

  final def task: Task[A] =
    Task.fromFuture(future).memoize

  final def future: Future[A] =
    promise.future

  final def isDefined = nonEmpty

  final def isEmpty = !nonEmpty

  final def nonEmpty = promise.future.isCompleted

  /**
   * Sets the variable.
   * Thread-safe.
   * May be called only once.
   *
   * @throws IllegalStateException
   */
  final def :=(value: A): A = {
    if (!trySet(value)) throw new IllegalStateException(s"SetOnce[$label] has already been set")
    value
  }

  /** @return true iff the value has not yet been set. */
  final def trySet(value: A): Boolean =
    promise.trySuccess(value)

  def orThrow: A =
    checked.orThrow

  final def getOrElse[B >: A](els: => B): B =
    checked getOrElse els

  final def checked: Checked[A] =
    promise.future.value match {
      case None => Left(Problem(s"SetOnce[$label] promise has not been kept so far"))
      case Some(o) => Right(o.get)
    }
}

object SetOnce
{
  import scala.language.implicitConversions

  def apply[A](implicit A: TypeTag[A]) = new SetOnce[A](A.tpe.toString)

  def apply[A](name: String) = new SetOnce[A](name)

  def fromOption[A](option: Option[A])(implicit A: TypeTag[A]) = {
    val setOnce = new SetOnce[A](A.tpe.toString)
    option foreach setOnce.:=
    setOnce
  }

  def fromOption[A](option: Option[A], label: String) = {
    val setOnce = new SetOnce[A](label)
    option foreach setOnce.:=
    setOnce
  }

  implicit def setOnceToOption[A](o: SetOnce[A]): Option[A] = o.toOption
}
