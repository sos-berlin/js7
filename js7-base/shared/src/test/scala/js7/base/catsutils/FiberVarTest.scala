package js7.base.catsutils

import cats.effect.{Deferred, FiberIO, IO}
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import js7.base.test.OurAsyncTestSuite
import org.scalatest.Assertion

final class FiberVarTest extends OurAsyncTestSuite:

  "FiberVar" in:
    Vector.tabulate(100)(identity).traverse(myTest).map(_.combineAll)

  private def myTest(i: Int): IO[Assertion] =
    IO.defer:
      val fiberVar = new FiberVar[Int]

      def newFiber(onCancel: => Unit): IO[FiberIO[Int]] =
        for
          whenStarted <- Deferred[IO, Unit]
          fiber <- whenStarted
            .complete(())
            .*>(IO.never)
            .as(1)
            .onCancel(IO(onCancel))
            .start
          _ <- whenStarted.get
        yield fiber

      @volatile var canceled1, canceled2, canceled3, canceled4 = false
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
        _ <- fiberVar.cancelCurrent
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
