package com.sos.jobscheduler.common.scalautil

import scala.concurrent.Promise
import scala.reflect.runtime.universe._
import scala.util.Success

/**
 * Variable which can be set only once. Thread-safe.
 *
 * @author Joacim Zschimmer
 */
class SetOnce[A](name: String)
{
  protected[this] val promise = Promise[A]

  /**
   * The value of the set variable.
   *
   * @throws IllegalStateException
   */
  def apply() = getOrElse { throw new IllegalStateException(s"SetOnce[$name] promise has not been kept so far") }

  final override def toString = toStringOr(s"SetOnce[$name](not yet set)")

  @inline final def toStringOr(or: => String): String =
    promise.future.value match {
      case None => or
      case Some(Success(o)) => o.toString
      case Some(o) => o.toString  // Never happens
    }

  @inline final def getOrElse[B >: A](els: => B): B =
    promise.future.value match {
      case None => els
      case Some(o) => o.get
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
    if (!promise.trySuccess(value)) throw new IllegalStateException(s"SetOnce[$name] has already been set")
    value
  }
}

object SetOnce
{
  import scala.language.implicitConversions

  def apply[A](implicit A: TypeTag[A]) = new SetOnce[A](A.tpe.toString)

  def fromOption[A](option: Option[A])(implicit A: TypeTag[A]) = {
    val setOnce = new SetOnce[A](A.tpe.toString)
    option foreach setOnce.:=
    setOnce
  }

  implicit def setOnceToOption[A](o: SetOnce[A]): Option[A] = o.toOption
}
