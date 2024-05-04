package js7.base.monixutils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.problem.Problem
import js7.base.test.OurAsyncTestSuite

final class AsyncVariableTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val asyncVariable = AsyncVariable(1)

  "get" in:
    assert(asyncVariable.get == 1)

  "value" in:
    asyncVariable
      .value
      .map(v => assert(v == 1))
      .unsafeToFuture()

  "set" in:
    asyncVariable.set(7)
      .map(v => assert(v == 7))
      .*>(IO(assert(asyncVariable.get == 7)))
      .*>(asyncVariable.set(1).as(succeed))
      .unsafeToFuture()

  "updated" in:
    asyncVariable.update(i => IO(i + 1))
      .map(v => assert(v == 2))
      .unsafeToFuture()

  "updatedChecked" - {
    "Left" in:
      asyncVariable.updateChecked(i => IO(Left(Problem("PROBLEM"))))
        .map(checked => assert(checked == Left(Problem("PROBLEM"))))
        .unsafeToFuture()

    "Right" in:
      asyncVariable.updateChecked(i => IO(Right(i + 1)))
        .map(v => assert(v == Right(3)))
        .unsafeToFuture()
  }

  "updateCheckedWithResult" - {
    "Left" in:
      asyncVariable
        .updateCheckedWithResult[Nothing](_ => IO(Left(Problem("PROBLEM"))))
        .map(checked =>
          assert(checked == Left(Problem("PROBLEM")) && asyncVariable.get == 3))
        .unsafeToFuture()

    "Right" in:
      asyncVariable
        .updateCheckedWithResult(i => IO(Right(i + 4 -> "hej")))
        .map(checked =>
          assert(checked == Right("hej") && asyncVariable.get == 7))
        .unsafeToFuture()
  }

  "toString" in:
    assert(asyncVariable.toString ==
      "js7.base.monixutils.AsyncVariableTest#asyncVariable: AsyncVariable[Int]")

  "Massive parallel" in:
    pending // TODO
