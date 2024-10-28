package js7.tests.cluster.controller

import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.problem.Checked.Ops
import js7.base.problem.ProblemException
import js7.base.thread.Futures.implicits.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.Problems.{AckFromActiveClusterNodeProblem, ClusterNodeIsNotActiveProblem}
import js7.data.controller.ControllerCommand
import js7.data.event.EventRequest
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId}
import js7.tests.cluster.controller.ControllerClusterTester.*
import js7.tests.testenv.ProgramEnvTester.assertEqualJournalFiles
import js7.base.monixlike.MonixLikeExtensions.{completedL, headL}

final class SimpleControllerClusterTest extends ControllerClusterTester:

  override protected def shellScript = s"echo '${"-" * 100}'\n" *
    (if sys.props.contains("test.speed") then 10000 else 1)

  "Cluster replicates journal files properly" in:
    runControllerAndBackup(): (primary, primaryController, _, backup, backupController, _, _) =>
      assert(primaryController.api.executeCommand(ControllerCommand.NoOperation()).await(99.s) ==
        Right(ControllerCommand.Response.Accepted))

      // Passive cluster node rejects command for active cluster node
      assert(backupController.api.executeCommand(ControllerCommand.NoOperation()).await(99.s) ==
        Left(ClusterNodeIsNotActiveProblem))

      assert(
        intercept[ProblemException]:
          primaryController.eventWatch.underlying.streamEventIds(timeout = Some(0.s))
            .await(99.s).orThrow.completedL.await(99.s)
        .problem == AckFromActiveClusterNodeProblem)

      backupController.eventWatch.underlying.streamEventIds(timeout = Some(0.s))
        .await(99.s).orThrow.completedL.await(99.s)

      val n = sys.props.get("test.speed").fold(1)(_.toInt)
      val orderIds = (1 to n).map(i => OrderId(s"🔶 $i"))
      val whenFinished = primaryController.eventWatch
        .stream(EventRequest.singleClass[OrderFinished](timeout = Some(88.s)))
        .scan(orderIds.toSet)(_ - _.value.key)
        .dropWhile(_.nonEmpty)
        .headL
        .unsafeToFuture()
      orderIds
        .map(orderId => primaryController.api
          .addOrder(FreshOrder(orderId, TestWorkflow.path)))
        .toVector.sequence
        .await(99.s)
        .sequence.orThrow
      whenFinished.await(99.s)

      assertEqualJournalFiles(primary.controllerEnv, backup.controllerEnv, n = 1)
