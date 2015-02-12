package com.sos.scheduler.engine.common.async

import java.util.concurrent.Callable

trait ShortTermCall[A] extends TimedCall[A] {
  final def epochMillis =
    TimedCall.ShortTermMillis
}

object ShortTermCall {
  def apply[A](f: () => A) =
    new ShortTermCall[A] {
      def call() = f()
    }

  def apply(r: Runnable): ShortTermCall[Unit] =
    new ShortTermCall[Unit] {
      def call() = r.run()
    }

  def apply[A](r: Callable[A]): ShortTermCall[A] =
    new ShortTermCall[A] {
      def call() = r.call()
    }
}
