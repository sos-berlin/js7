package com.sos.scheduler.engine.common.scalautil

import java.util.concurrent.atomic.AtomicReference

/**
 * Variable which can be set only once. Thread-safe.
 *
 * @author Joacim Zschimmer
 */
final class SetOnce[A] {
  private val ref = new AtomicReference[A]

  /**
   * The value of the set variable.
   *
   * @throws IllegalStateException
   */
  def apply() = getOrElse { throw new IllegalStateException("Value is not yet set") }

  override def toString = toStringOr("(not yet set)")

  @inline def toStringOr(or: ⇒ String): String =
    ref.get() match {
      case null ⇒ or
      case o ⇒ o.toString
    }

  @inline def getOrElse[B >: A](els: ⇒ B) =
    ref.get() match {
      case null ⇒ els
      case o ⇒ o
    }

  def get = Option(ref.get())

  def isDefined = nonEmpty

  def nonEmpty = ref.get != null

  def isEmpty = ref.get == null

  /**
   * Sets the variable.
   * Thread-safe.
   * May be called only once.
   *
   * @throws IllegalStateException
   */
  def :=(value: A): Unit = {
    val ok = ref.compareAndSet(null.asInstanceOf[A], value)
    if (!ok) throw new IllegalStateException(s"SetOnce[${ref.get.getClass.getName}] has already been set")
  }
}
