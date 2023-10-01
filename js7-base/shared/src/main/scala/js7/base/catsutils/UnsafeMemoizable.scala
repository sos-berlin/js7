package js7.base.catsutils

import cats.effect.concurrent.Deferred
import cats.effect.syntax.syncEffect.*
import cats.effect.{Concurrent, SyncEffect, SyncIO}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import java.util.concurrent.atomic.AtomicBoolean
import js7.base.utils.Lazy
import monix.eval.Task

/** Monix Task like unsafe memoize for any Cats Concurrent. */
trait UnsafeMemoizable[F[_]]:
  extension[A](o: F[A])
    def unsafeMemoize: F[A]


object UnsafeMemoizable:

  given taskMemoizable: UnsafeMemoizable[Task] with
    extension[A](task: Task[A])
      def unsafeMemoize: Task[A] =
        task.memoize

  given concurrentMemoizable[F[_]](using F: Concurrent[F]): UnsafeMemoizable[F] with
    extension[A](leftSide: F[A])
      def unsafeMemoize: F[A] =
        val triggered = new AtomicBoolean(false)
        val deferred = Deferred.unsafe[F, A]
        F.defer:
          if !triggered.getAndSet(true) then
            leftSide.flatMap(a => deferred.complete(a).as(a))
          else
            deferred.get

  given syncEffectMemoizable[F[_]](using F: SyncEffect[F]): UnsafeMemoizable[F] with
    extension[A](leftSide: F[A])
      def unsafeMemoize: F[A] =
        // `Lazy` blocks the thread when used concurrently !!!
        val lzy = Lazy(leftSide.runSync[SyncIO].unsafeRunSync())
        F.delay(lzy.value)
