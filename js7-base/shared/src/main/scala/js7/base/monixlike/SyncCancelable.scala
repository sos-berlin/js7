package js7.base.monixlike

import js7.base.utils.EmptyRunnable

/** May describe callback which cancels a timer. */
trait SyncCancelable:
  def cancel(): Unit


object SyncCancelable:

  val empty: SyncCancelable = apply(EmptyRunnable)

  def apply(cancel: Runnable): SyncCancelable =
    val cancel_ = cancel
    new SyncCancelable:
      def cancel(): Unit = cancel_.run()
