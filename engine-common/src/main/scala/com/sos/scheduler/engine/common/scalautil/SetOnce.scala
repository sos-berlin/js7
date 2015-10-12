package com.sos.scheduler.engine.common.scalautil

import java.util.concurrent.atomic.AtomicReference

/**
 * @author Joacim Zschimmer
 */
final class SetOnce[A] {
  private val value = new AtomicReference[A]

  def apply() = getOrElse { throw new IllegalStateException("Value is not yet set") }

  override def toString = toStringOr("(not yet set)")

  @inline def toStringOr(or: ⇒ String): String =
    value.get() match {
      case null ⇒ or
      case o ⇒ o.toString
    }

  @inline def getOrElse[B >: A](els: ⇒ B) =
    value.get() match {
      case null ⇒ els
      case o ⇒ o
    }

  def get = Option(value.get())

  def isEmpty = value == null

  def nonEmpty = value == null

  def isDefined = nonEmpty

  def :=(a: A): Unit = {
    val ok = value.compareAndSet(null.asInstanceOf[A], a)
    if (!ok) throw new IllegalStateException("Already set")
  }
}
