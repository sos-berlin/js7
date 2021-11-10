package js7.base.monixutils

import js7.base.problem.Problem
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class AsyncVariableTest extends AsyncFreeSpec
{
  private val asyncVariable = AsyncVariable(1)

  "get" in {
    assert(asyncVariable.get == 1)
  }

  "updateCheckedWithResult" - {
    "Left" in {
      asyncVariable
        .updateCheckedWithResult[Nothing](i => Task(Left(Problem("PROBLEM"))))
        .map(checked =>
          assert(checked == Left(Problem("PROBLEM")) && asyncVariable.get == 1))
        .runToFuture
    }

    "Right" in {
      asyncVariable
        .updateCheckedWithResult(i => Task(Right(i + 3 -> "hej")))
        .map(checked =>
          assert(checked == Right("hej") && asyncVariable.get == 4))
        .runToFuture
    }
  }
}
