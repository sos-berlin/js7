package js7.base.monixutils

import js7.base.problem.Problem
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite

final class AsyncVariableTest extends OurAsyncTestSuite
{
  private val asyncVariable = AsyncVariable(1)

  "get" in {
    assert(asyncVariable.get == 1)
  }

  "updated" in {
    asyncVariable.update(i => Task(i + 1))
      .map(v => assert(v == 2))
      .runToFuture
  }

  "updatedChecked" - {
    "Left" in {
      asyncVariable.updateChecked(i => Task(Left(Problem("PROBLEM"))))
        .map(checked => assert(checked == Left(Problem("PROBLEM"))))
        .runToFuture
    }

    "Right" in {
      asyncVariable.updateChecked(i => Task(Right(i + 1)))
        .map(v => assert(v == Right(3)))
        .runToFuture
    }
  }

  "updateCheckedWithResult" - {
    "Left" in {
      asyncVariable
        .updateCheckedWithResult[Nothing](_ => Task(Left(Problem("PROBLEM"))))
        .map(checked =>
          assert(checked == Left(Problem("PROBLEM")) && asyncVariable.get == 3))
        .runToFuture
    }

    "Right" in {
      asyncVariable
        .updateCheckedWithResult(i => Task(Right(i + 4 -> "hej")))
        .map(checked =>
          assert(checked == Right("hej") && asyncVariable.get == 7))
        .runToFuture
    }
  }

  "Massive parallel" in {
    pending // TODO
  }
}
