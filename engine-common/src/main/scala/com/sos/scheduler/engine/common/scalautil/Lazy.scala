package com.sos.scheduler.engine.common.scalautil

/**
  * Like Scala lazy but synchronization moved to own object.
  *
  * @author Joacim Zschimmer
  */
trait Lazy[A] extends (() ⇒ A)

object Lazy {
  def apply[A](lazyCompute: ⇒ A): Lazy[A] =
    new Lazy[A] {
      def apply(): A = lazyValue
      protected lazy val lazyValue = lazyCompute
    }
}
