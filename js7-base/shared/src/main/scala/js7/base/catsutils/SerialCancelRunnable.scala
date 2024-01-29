package js7.base.catsutils

import js7.base.utils.{Atomic, EmptyRunnable}

/** Mutable for cancel operation as Runnables. */
final class SerialCancelRunnable:

  private val _runnable = Atomic(EmptyRunnable: Runnable)

  def cancel(): Unit =
    this := EmptyRunnable

  def :=(runnable: Runnable): Unit =
    _runnable.getAndSet(runnable).run()
