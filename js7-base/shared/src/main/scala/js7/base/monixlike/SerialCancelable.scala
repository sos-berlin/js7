package js7.base.monixlike

import js7.base.log.Logger
import js7.base.utils.Atomic
import scala.annotation.targetName
import scala.concurrent.Future

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
final class SerialCancelable(initial: Cancelable = Cancelable.empty)
extends Cancelable:

  @volatile private var canceled = false
  private val state = Atomic(initial)

  def unsafeCancelAndForget(): Unit =
    canceled = true
    state.get().unsafeCancelAndForget()

  //def unsafeBlockingCancel(): Unit =
  //  canceled = true
  //  blockingCancel(state.get())

  @targetName("set")
  def :=(cancelable: () => Future[Unit]): this.type =
    :=(Cancelable(cancelable))

  @targetName("set")
  def :=(cancelable: Cancelable): this.type =
    state.getAndSet(cancelable).unsafeCancelAndForget()
    if canceled then cancelable.unsafeCancelAndForget()
    this

  //private def blockingCancel(cancelable: Cancelable) =
  //  val cancelingFuture = cancelable.unsafeCancel()
  //  blocking:
  //    try Await.result(cancelingFuture, Duration.Inf)
  //    catch case NonFatal(t) =>
  //      logger.error(s"${enclosing.value}.unsafeCancel => ${t.toStringWithCauses}")


object SerialCancelable:

  private val logger = Logger[this.type]
