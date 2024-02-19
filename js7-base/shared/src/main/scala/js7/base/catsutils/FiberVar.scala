package js7.base.catsutils

import cats.effect.{FiberIO, IO}
import java.util.concurrent.atomic.AtomicReference
import js7.base.utils.CatsUtils.canceledFiberIO
import js7.base.utils.{Atomic, CatsUtils}

final class FiberVar[A]:

  @volatile private var canceled = false
  private val _fiber = Atomic(canceledFiberIO[A])

  def isCanceled: Boolean =
    canceled

  def cancel: IO[Unit] =
    IO.defer:
      canceled = true
      _fiber.getAndSet(canceledFiberIO).cancel

  def set(fiber: FiberIO[A] = canceledFiberIO): IO[Unit] =
    IO.defer:
      if canceled then
        fiber.cancel
      else
        _fiber.getAndSet(fiber).cancel
