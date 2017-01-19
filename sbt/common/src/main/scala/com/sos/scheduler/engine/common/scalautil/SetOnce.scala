package com.sos.scheduler.engine.common.scalautil

import java.util.Objects.requireNonNull
import java.util.concurrent.atomic.AtomicReference

/**
 * Variable which can be set only once. Thread-safe.
 *
 * @author Joacim Zschimmer
 */
class SetOnce[A](name: String = "SetOnce") {
  protected[this] val ref = new AtomicReference[A]

  /**
   * The value of the set variable.
   *
   * @throws IllegalStateException
   */
  def apply() = getOrElse { throw new IllegalStateException(s"$name has not been set") }

  final override def toString = toStringOr("(not yet set)")

  @inline final def toStringOr(or: ⇒ String): String =
    ref.get() match {
      case null ⇒ or
      case o ⇒ o.toString
    }

  @inline final def getOrElse[B >: A](els: ⇒ B) =
    ref.get() match {
      case null ⇒ els
      case o ⇒ o
    }

  final def getOrUpdate(value: ⇒ A) =
    if (ref.get != null)
      ref.get
    else {
      trySet(value)   // When concurrently called, the value is discarded
      ref.get
    }

  final def get = toOption

  final def toOption = Option(ref.get())

  final def isDefined = nonEmpty

  final def nonEmpty = ref.get != null

  final def isEmpty = ref.get == null

  /**
   * Sets the variable.
   * Thread-safe.
   * May be called only once.
   *
   * @throws IllegalStateException
   */
  final def :=(value: A): A = {
    val ok = trySet(value)
    if (!ok) throw new IllegalStateException(s"SetOnce[${ref.get.getClass.getName}] has already been set")
    value
  }

  final def trySet(value: A): Boolean =
    ref.compareAndSet(null.asInstanceOf[A], requireNonNull(value))   // Returns false on concurrent execution. Then value is discarded
}

object SetOnce {
  import scala.language.implicitConversions

  implicit def setOnceToOption[A](o: SetOnce[A]): Option[A] = o.toOption

  /**
    * Makes a SetOnce implicitly readable.
    * <pre>
    * val once = new SetOnce[Int] with SetOnce.Implicit
    * ...
    * val i: Int = once
    * </pre>
    */
  trait Implicit {
    this: SetOnce[_] ⇒
  }

  object Implicit {
    implicit def dereference[A](a: SetOnce[A] with Implicit): A = a()
  }
}
