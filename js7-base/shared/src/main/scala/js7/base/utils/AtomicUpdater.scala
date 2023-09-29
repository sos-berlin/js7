package js7.base.utils

import monix.execution.atomic.AtomicAny
import scala.annotation.tailrec

final class AtomicUpdater[A <: AnyRef](initial: A)
{
  private val value = AtomicAny(initial)

  def get: A =
    value.get()

  @tailrec
  def update(f: A => A): Unit = {
    val a = value.get()
    if !value.compareAndSet(a, f(a)) then
      update(f)
  }

  def getAndSet(a: A): A =
    value.getAndSet(a)
}
