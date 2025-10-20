package js7.base.monixlike

import js7.base.monixlike.FutureCancelableBlocking.blockingCancel
import js7.base.scalasource.ScalaSourceLocation
import js7.base.utils.{Atomic, CancelableFuture}
import scala.annotation.targetName
import scala.concurrent.Future

/** Inspired vom Monix SerialFutureCancelable.
  *
  * Represents a [[monix.execution.Cancelable]] whose underlying cancelable
  * can be swapped for another cancelable which causes the previous underlying
  * cancelable to be canceled.
  *
  * Example:
  * {{{
  *   val s = SerialFutureCancelable()
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
@deprecated("blocking")
final class SerialFutureCancelable(initial: FutureCancelable = FutureCancelable.empty)
extends FutureCancelable:

  @volatile private var canceled = false
  private val state = Atomic(initial)

  def cancelToFuture(): Future[Unit] =
    canceled = true
    state.getAndSet(FutureCancelable.empty).cancelToFuture()

  @targetName("set")
  def :=(future: CancelableFuture[?])(using sourcecode.Enclosing, ScalaSourceLocation): this.type =
    this := (() => future.cancelToFuture())

  @targetName("set")
  def :=(cancelable: () => Future[Unit])(using sourcecode.Enclosing, ScalaSourceLocation)
  : this.type =
    this := FutureCancelable(cancelable)

  @targetName("set")
  def :=(cancelable: FutureCancelable)(using sourcecode.Enclosing, ScalaSourceLocation): this.type =
    if canceled then
      cancelable.blockingCancel()
    else
      state.getAndSet(cancelable).blockingCancel()
    this
