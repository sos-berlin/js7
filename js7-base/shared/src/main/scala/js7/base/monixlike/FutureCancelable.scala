package js7.base.monixlike

import scala.concurrent.Future

trait FutureCancelable:

  def cancelToFuture(): Future[Unit]

  final def cancelAndForget(): Unit =
    cancelToFuture()


object FutureCancelable:

  val empty: FutureCancelable =
    EmptyFutureCancelable

  def apply(cancel: () => Future[Unit]): StandardFutureCancelable =
    StandardFutureCancelable(cancel)


final class StandardFutureCancelable(f: () => Future[Unit]) extends FutureCancelable:

  def cancelToFuture(): Future[Unit] =
    f()


object EmptyFutureCancelable extends FutureCancelable:

  def cancelToFuture(): Future[Unit] = Future.successful(())
