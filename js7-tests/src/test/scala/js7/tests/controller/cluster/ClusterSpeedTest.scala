package js7.tests.controller.cluster

import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteOrdersWhenTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.cluster.ClusterSpeedTest.*
import js7.tests.testenv.ControllerClusterForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.Deadline.now

final class ClusterSpeedTest extends OurTestSuite with ControllerClusterForScalaTest
{
  val items = Seq(workflow)

  for (n <- sys.props.get("test.speed"/*try 1000*/).map(_.toInt)) {
    "Speed test" in {
      withControllerAndBackup() { (primary, backup, _) =>
        val backupController = backup.startController(httpPort = Some(backupControllerPort)) await 99.s
        val primaryController = primary.startController(httpPort = Some(primaryControllerPort))
          .await(99.s)
        primaryController.eventWatch.await[ClusterEvent.ClusterCoupled]()

        val orderIdIterator = Iterator.from(1).map(i => OrderId(i.toString))
        def cycle = Task.defer {
          val orderId = orderIdIterator.synchronized(orderIdIterator.next())
          val order = FreshOrder(orderId, workflow.path)
          // 3 acks:
          for {
            _ <- primaryController.addOrder(order).map(_.orThrow)
            _ <- primaryController.executeCommandAsSystemUser(CancelOrders(Seq(order.id)))
              .map(_.orThrow)
            _ <- primaryController.executeCommandAsSystemUser(DeleteOrdersWhenTerminated(Seq(order.id)))
              .map(_.orThrow)
          } yield ()
        }
        // Warm up
        Task.parSequence((1 to 1000).map(_ => cycle)).await(99.s)

        val t = now
        Task.parSequence((1 to n).map(_ => cycle)).await(99.s)
        info(Stopwatch.itemsPerSecondString(t.elapsed, 3 * n, "acks"))

        primaryController.terminate() await 99.s
        backupController.terminate() await 99.s
      }
    }
  }
}

object ClusterSpeedTest
{
  private val workflow = Workflow(WorkflowPath("SPEED") ~ "INITIAL",
    Seq(Prompt(StringConstant(""))))
}
