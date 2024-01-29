package js7.base.monixlike

import js7.base.utils.{Atomic, EmptyRunnable}
import scala.annotation.targetName

final class SerialSyncCancelable(initial: SyncCancelable = SyncCancelable.empty)
  extends SyncCancelable:

  @volatile private var canceled = false
  private val state = Atomic(initial)

  def cancel(): Unit =
    canceled = true
    this := EmptyRunnable

  @targetName("set")
  def :=(cancel: Runnable): this.type =
    this := SyncCancelable(cancel)

  @targetName("set")
  def :=(cancelable: SyncCancelable): this.type =
    if canceled then
      cancelable.cancel()
    else
      state.getAndSet(cancelable).cancel()
    this

  def isCanceled: Boolean =
    canceled