package js7.base.catsutils

import cats.effect.{IO, Outcome}
import js7.base.test.OurAsyncTestSuite

final class FiberVarTest extends OurAsyncTestSuite:

  "FiberVar" in:
    val fiberVar = new FiberVar[Int]
    assert(!fiberVar.isCanceled)

    def newFiber(onCancel: => Unit) =
      IO.never.as(1)
        .guaranteeCase:
          case Outcome.Canceled() => IO(onCancel)
          case _ => IO.unit
        .start

    var canceled1, canceled2, canceled3, canceled4 = false
    for
      // Initial set
      fiber <- newFiber { canceled1 = true }
      _ <- fiberVar.set(fiber)
      _ = assert(!canceled1)

      // Second set cancels the first fiber
      fiber <- newFiber { canceled2 = true }
      _ <- fiberVar.set(fiber)
      _ = assert(canceled1 && !canceled2)

      // Cancel the last fiber
      _ <- fiberVar.set()
      _ = assert(canceled2)

      // Cancel the fiberVar itself and any future fiber
      fiber <- newFiber { canceled3 = true }
      _ <- fiberVar.set(fiber)
      _ <- fiberVar.cancel
      _ = assert(canceled3)

      // Because fiberVar itself has been canceled, any new Fiber will be canceled immediately
      fiber <- newFiber { canceled4 = true }
      _ <- fiberVar.set(fiber)
      _ = assert(canceled4)
    yield
      succeed
