package js7.base.catsutils

import cats.effect.concurrent.Deferred
import cats.effect.syntax.syncEffect.*
import cats.effect.{Concurrent, Sync, SyncEffect, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.utils.Lazy
import monix.eval.Task

/** Monix Task like unsafe memoize for any Cats Concurrent. */
trait UnsafeMemoizable[F[_]]:

  def unsafeMemoize[A](f: F[A]): F[A]

object UnsafeMemoizable:
  def apply[F[_]](implicit o: UnsafeMemoizable[F]) = o

  implicit val taskMemoizable: UnsafeMemoizable[Task] =
    new UnsafeMemoizable[Task]:
      def unsafeMemoize[A](task: Task[A]) = task.memoize

  implicit def concurrentMemoizable[F[_]: Concurrent]: UnsafeMemoizable[F] =
    new UnsafeMemoizable[F]:
      def unsafeMemoize[A](body: F[A]): F[A] =
        val triggered = new AtomicBoolean(false)
        val deferred = Deferred.unsafe[F, A]

        Concurrent[F].defer:
          if !triggered.getAndSet(true) then
            body.flatMap(a => deferred.complete(a).as(a))
          else
            deferred.get

  implicit def blockingSyncEffectMemoizable[F[_] : SyncEffect]: UnsafeMemoizable[F] =
    new UnsafeMemoizable[F]:
      def unsafeMemoize[A](body: F[A]): F[A] =
        // `Lazy` blocks the thread when used concurrently. Only a experiment.
        val lzy = Lazy(body.runSync[SyncIO].unsafeRunSync())
        Sync[F].delay(lzy.value)

  object syntax:
    implicit class RichMemoizable[F[_], A](private val f: F[A]) extends AnyVal:
      def unsafeMemoize(implicit F: UnsafeMemoizable[F]): F[A] =
        F.unsafeMemoize(f)
