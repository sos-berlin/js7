package js7.base.monixlike

import scala.concurrent.Future

trait Cancelable:

  @deprecated("Use unsafeCancelAndForget()")
  final def cancel(): Unit =
    unsafeCancelAndForget()

  def unsafeCancelAndForget(): Unit


object Cancelable:
  val empty: Cancelable =
    SimpleCancelable(() => ())

  def apply(cancel: () => Future[Unit]): Cancelable =
    SimpleCancelable(cancel.asInstanceOf[() => Unit])

  def fromRunnable(cancel: Runnable): Cancelable =
    SimpleCancelable(cancel.run)


/** This is what IO#unsafeRunCancelable returns. */
final class SimpleCancelable(f: () => Unit) extends Cancelable:

  def unsafeCancelAndForget(): Unit =
    f()
