package js7.base.utils

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import js7.base.monixlike.MonixLikeExtensions.unsafeToCancelableFuture
import js7.base.test.OurAsyncTestSuite
import js7.base.test.ScalaTestForCats.given
import js7.base.time.ScalaTime.*
import org.scalatest.compatible.Assertion

final class CancelableFutureTest extends OurAsyncTestSuite:

  "cancel" in:
    val myTest =
      for
        started <- Deferred[IO, Unit]
        canceled <- Ref.of[IO, Boolean](false)
        future = started.complete(()).void
          .andWait(1000.h) // Block eternally
          .onCancel(canceled.set(true))
          .unsafeToCancelableFuture()
        _ <- started.get
        _ <- IO.fromFuture(IO(future.cancel()))
        _ <- IO.fromFuture(IO(future)).attempt/*because the fiber has been canceled*/
        isCanceled <- canceled.get
      yield assert(future.isCompleted && isCanceled)

    Seq.from(1 to 1000)
      .parTraverse(_ => myTest)
      .map(_.combineAll)
