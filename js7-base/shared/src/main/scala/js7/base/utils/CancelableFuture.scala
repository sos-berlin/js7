package js7.base.utils

import js7.base.monixlike.FutureCancelable
import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.Try

final class CancelableFuture[A](future: Future[A], cancelToFuture_ : () => Future[Unit])
extends Future[A], FutureCancelable:

  def onComplete[U](f: Try[A] => U)(using ExecutionContext): Unit =
    future.onComplete(f)

  def isCompleted: Boolean =
    future.isCompleted

  def value: Option[Try[A]] =
    future.value

  def transform[S](f: Try[A] => Try[S])(using ExecutionContext): CancelableFuture[S] =
    CancelableFuture(
      future.transform(f),
      cancelToFuture)

  def transformWith[B](f: Try[A] => Future[B])(using ExecutionContext): Future[B] =
    CancelableFuture(
      future.transformWith(f),
      cancelToFuture)

  def ready(atMost: Duration)(using CanAwait): this.type =
    future.ready(atMost)
    this

  def result(atMost: Duration)(using CanAwait): A =
    future.result(atMost)

  def cancelToFuture(): Future[Unit] =
    cancelToFuture_()


object CancelableFuture:

  def fromPair[A](pair: (Future[A], () => Future[Unit])): CancelableFuture[A] =
    new CancelableFuture(pair._1, pair._2)
