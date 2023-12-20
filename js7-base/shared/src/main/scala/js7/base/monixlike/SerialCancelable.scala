package js7.base.monixlike

import js7.base.utils.{Atomic, EmptyRunnable}
import scala.annotation.{tailrec, targetName}

/** Inspired vom Monix SerialCancelable.
  *
  * Represents a [[monix.execution.Cancelable]] whose underlying cancelable
  * can be swapped for another cancelable which causes the previous underlying
  * cancelable to be canceled.
  *
  * Example:
  * {{{
  *   val s = SerialCancelable()
  *   s := c1 // sets the underlying cancelable to c1
  *   s := c2 // cancels c1 and swaps the underlying cancelable to c2
  *
  *   s.cancel() // also cancels c2
  *
  *   s := c3 // also cancels c3, because s is already canceled
  * }}}
  *
  * Also see [[OrderedCancelable]], which is similar, but doesn't cancel
  * the old cancelable upon assignment.
  */
final class SerialCancelable(initial: Runnable):
  private val state = Atomic(initial)

  def isCanceled: Boolean =
    state.get() match
      case null => true
      case _ => false

  def cancel(): Unit =
    state.getAndSet(null) match
      case null => () // nothing to do
      case ref => ref.run()

  @tailrec @targetName("set")
  def :=(cancel: Runnable): this.type =
    val current = state.get()
    if !state.compareAndSet(current, cancel) then
      :=(cancel) // retry
    else
      current.run()
      this


object SerialCancelable:
  def apply(): SerialCancelable =
    new SerialCancelable(EmptyRunnable)
  //
  ///** Builder for [[SerialCancelable]]. */
  //def apply(initial: Runnable): SerialCancelable =
  //  new SerialCancelable(initial)
