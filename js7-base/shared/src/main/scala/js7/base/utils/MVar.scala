package js7.base.utils

import cats.effect.std.{Mutex, Queue}
import cats.effect.{Async, Concurrent, Deferred, GenTemporal, Ref, Sync}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.{Defer, FlatMap}
import io.github.timwspence.cats.stm.STM
import java.util.concurrent.atomic.AtomicReference
import js7.base.catsutils.UnsafeMemoizable.given
import js7.base.log.Logger
import js7.base.monixutils.SimpleLock
import js7.base.utils.MVar.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable.ListBuffer

final class MVar[F[_], A] private(stm: STM[F])(using F: Async[F]):

  private val tmvar = stm.TMVar.empty[A]

  def put(a: A): F[Unit] =
    stm.commit:
      tmvar.flatMap(_.put(a))

  def tryPut(a: A): F[Boolean] =
    stm.commit:
      tmvar.flatMap(_.tryPut(a))

  def take: F[A] =
    stm.commit:
      tmvar.flatMap(_.take)

  def tryTake: F[Option[A]] =
    stm.commit:
      tmvar.flatMap(_.tryTake)

  def read: F[A] =
    stm.commit:
      tmvar.flatMap(_.read)

  def tryRead: F[Option[A]] =
    stm.commit:
      tmvar.flatMap(_.tryRead)

  def isEmpty: F[Boolean] =
    stm.commit:
      tmvar.flatMap(_.isEmpty)


object MVar:
  private val logger = Logger[this.type]

  def apply[F[_]]: Builder[F] =
    new Builder[F]

  final class Builder[F[_]] private[MVar]:
    def of[A](initial: A)(using F: Async[F]): F[MVar[F, A]] =
      empty[A]
        .flatMap(mvar => mvar.tryPut(initial).as(mvar))

    def empty[A](using F: Async[F]): F[MVar[F, A]] =
      MVar.empty[F, A]

  def empty[F[_], A](using F: Async[F]): F[MVar[F, A]] =
    for
      stm <- STM.runtime[F]
      mvar <- F.delay(new MVar[F, A](stm))
    yield mvar
