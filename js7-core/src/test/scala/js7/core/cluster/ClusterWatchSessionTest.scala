package js7.core.cluster

import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.core.cluster.ClusterWatchSession.OutdatedRequestProblem
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec

final class ClusterWatchSessionTest extends AnyFreeSpec
{
  "withRequestId" in {
    val inlay = ClusterWatchSession.newInlay()
    assert(inlay.withRequestId(0)(Task.right(7)).await(99.s) == Right(7))
    assert(inlay.withRequestId(1)(Task.right(7)).await(99.s) == Right(7))
    assert(inlay.withRequestId(2)(Task.left(Problem("X"))).await(99.s) == Left(Problem("X")))
    assert(inlay.withRequestId(2)(Task.right(7)).await(99.s) == Left(OutdatedRequestProblem))
    assert(inlay.withRequestId(1)(Task.right(7)).await(99.s) == Left(OutdatedRequestProblem))
    assert(inlay.withRequestId(3)(Task.right(7)).await(99.s) == Right(7))
  }
}
