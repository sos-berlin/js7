package js7.tests.controller.cluster

import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.problem.Checked.Ops
import js7.base.problem.{Problem, ProblemException}
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.Problems.ClusterNodeIsNotActiveProblem
import js7.data.controller.ControllerCommand
import js7.data.event.EventRequest
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.controller.cluster.ControllerClusterTester.*
import js7.tests.testenv.ControllerClusterForScalaTest.assertEqualJournalFiles
import monix.execution.Scheduler.Implicits.traced

final class SimpleClusterTest extends ControllerClusterTester
{
  override protected def shellScript = s"echo '${"-" * 100}'\n" *
    (if (sys.props.contains("test.speed")) 10000 else 1)

  "Cluster replicates journal files properly" in {
    runControllerAndBackup() { (primary, primaryController, backup, backupController, _) =>
      assert(primaryController.executeCommandForTest(ControllerCommand.NoOperation()) ==
        Right(ControllerCommand.Response.Accepted))

      // Passive cluster node rejects command for active cluster node
      assert(backupController.executeCommandForTest(ControllerCommand.NoOperation()) ==
        Left(ClusterNodeIsNotActiveProblem))

      assert(
        intercept[ProblemException] {
          primaryController.eventWatch.underlying.observeEventIds(timeout = Some(0.s)).completedL await 99.s
        }.problem == Problem("This active cluster node does not provide event acknowledgements (two active cluster nodes?)"))

      backupController.eventWatch.underlying.observeEventIds(timeout = Some(0.s)).completedL await 99.s

      val n = sys.props.get("test.speed").fold(1)(_.toInt)
      val orderIds = (1 to n).map(i => OrderId(s"🔶 $i"))
      val whenFinished = primaryController.eventWatch
        .observe(EventRequest.singleClass[OrderFinished](timeout = Some(88.s)))
        .scan(orderIds.toSet)(_ - _.value.key)
        .dropWhile(_.nonEmpty)
        .headL
        .runToFuture
      orderIds
        .map(orderId => primaryController.addOrder(FreshOrder(orderId, TestWorkflow.path)))
        .toVector.sequence
        .await(99.s)
        .sequence.orThrow
      whenFinished await 99.s

      assertEqualJournalFiles(primary.controller, backup.controller, n = 1)
    }
  }
}
