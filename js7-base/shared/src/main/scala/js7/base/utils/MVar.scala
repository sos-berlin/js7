package js7.base.utils

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.github.timwspence.cats.stm.STM
import js7.base.utils.MVar.*

final class MVar[F[_], A] private(stm: STM[F])(tmvar: stm.TMVar[A]):

  def put(a: A): F[Unit] =
    stm.commit:
      tmvar.put(a)

  def tryPut(a: A): F[Boolean] =
    stm.commit:
      tmvar.tryPut(a)

  def take: F[A] =
    stm.commit:
      tmvar.take

  def tryTake: F[Option[A]] =
    stm.commit:
      tmvar.tryTake

  def read: F[A] =
    stm.commit:
      tmvar.read

  def tryRead: F[Option[A]] =
    stm.commit:
      tmvar.tryRead

  def isEmpty: F[Boolean] =
    stm.commit:
      tmvar.isEmpty


object MVar:

  def apply[F[_]]: Builder[F] =
    new Builder[F]

  final class Builder[F[_]] private[MVar]:
    def of[A](initial: A)(using F: Async[F]): F[MVar[F, A]] =
      empty[A].flatMap: mvar =>
        mvar.tryPut(initial).as(mvar)

    def empty[A](using F: Async[F]): F[MVar[F, A]] =
      MVar.empty[F, A]

  def empty[F[_], A](using F: Async[F]): F[MVar[F, A]] =
    for
      stm <- STM.runtime[F]
      mvar <- stm.commit:
        for tmvar <- stm.TMVar.empty[A] yield
          new MVar[F, A](stm)(tmvar)
    yield
      mvar
